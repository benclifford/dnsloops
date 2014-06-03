{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Applicative
import Data.ByteString.Char8 (pack, split, intercalate)
import Data.Foldable (asum, Foldable)
import Data.IP
import Data.List (tails)
import Data.Typeable

import Network.DNS

import Q
import Control.Monad

-- | terminology from RFC1035
type LDomain = [LLabel]

type LLabel = String

data ResolverQuery = ResolverQuery ResolvConf Domain TYPE deriving (Show, Eq, Typeable)
data ResolverAnswer = ResolverAnswer (Either DNSError DNSFormat) deriving (Show, Eq, Typeable)

instance Qable ResolverQuery ResolverAnswer where
  runQable q@(ResolverQuery rc d t) = do
    result <- unsafeQT $ do
      resolver <- makeResolvSeed rc
      withResolver resolver $ \r -> lookupRaw r d t
    return ()
    qrecord q (ResolverAnswer result)

-- TODO: this can hopefully supercede GetNameserver more generally.
data GetRRSetQuery = GetRRSetQuery Domain TYPE deriving (Show, Eq, Typeable)
data GetRRSetAnswer = GetRRSetAnswer (Either String [ResourceRecord]) deriving (Show, Eq, Typeable)

instance Qable GetRRSetQuery GetRRSetAnswer where
  runQable q = error "Qable GetRRSetQuery NOTIMPL"

data GetNameserverQuery = GetNameserverQuery Domain deriving (Show, Eq, Typeable)
data GetNameserverAnswer = GetNameserverAnswer Domain deriving (Show, Eq, Typeable)

instance Qable GetNameserverQuery GetNameserverAnswer where
  runQable q@(GetNameserverQuery d) = do
    result <- unsafeQT $ do
      resolver <- makeResolvSeed defaultResolvConf
      withResolver resolver $ \r -> lookupNS r d
    case result of
      Right nameservers -> forM_ nameservers $ \ns -> qrecord q (GetNameserverAnswer ns)
      Left _ -> return () -- TODO: what should I do in the case of error? depends on the error. For now, silent discard

-- | these instances are needed to support Show and Eq on ResolverQuery and ResolverAnswer
deriving instance Show ResolvConf
deriving instance Eq ResolvConf
deriving instance Show FileOrNumericHost
deriving instance Eq FileOrNumericHost


main = do
  putStrLn "DNSLoops main"

{-
  resolver <- makeResolvSeed defaultResolvConf
  result <- withResolver resolver $ \r -> lookupRaw r hostname A
  print result 
-}

--  runQ $ resolve ["www", "hawaga", "org", "uk"]

  let hostname = pack "www.hawaga.org.uk"

  putStrLn "============ Test 1 ============"
  res <- runQ (simpleQuery hostname)
  putStrLn "Final result in Main: "
  print res

  putStrLn "============ Test 2 ============"
  res <- runQ $ populateRootHints <|> (complexResolve hostname)

  putStrLn "Final result in Main: "
  print res


simpleQuery hostname = query (ResolverQuery defaultResolvConf hostname A)

populateRootHints = 
      (qrecord (GetNameserverQuery $ pack "")
               (GetNameserverAnswer $ pack "a.root-servers.net") *> empty)
  <|> (qrecord (GetRRSetQuery aName A)
               (GetRRSetAnswer $ Right [ResourceRecord aName A 0 noLen (RD_A aIP)]) *> empty)
  where aName = pack "a.root-servers.net"
        aIP = toIPv4 [198,41,0,4]
        noLen = -1

-- I wonder if qrecord should end empty/mzero rather than returning a single () ?
-- Its probably nice to be able to use it sequentially in a do block though?
  -- TODO: prepopulate our seatbelt root resolver information,
  -- a subset of /domain/named.cache
{- 
.                        3600000  IN  NS    A.ROOT-SERVERS.NET.
A.ROOT-SERVERS.NET.      3600000      A     198.41.0.4
A.ROOT-SERVERS.NET.      3600000      AAAA  2001:503:BA3E::2:30
-}

forA_ :: (Foldable t, Functor t, Alternative a) => t x -> (x -> a y) -> a y
forA_ l f = asum (fmap f l)

complexResolve hostname = do

  let shreddedDomain = split '.' hostname 
  -- ^ BUG; handling of . inside labels
  let domainSuffixes = tails shreddedDomain
  report $ "Domain suffixes: " ++ (show domainSuffixes)

  forA_ domainSuffixes $ \domainSuffix -> do
    let domainSuffixByteString = intercalate (pack ".") domainSuffix
    -- ^ BUG: handling of . inside labels
    -- cache lookup

    -- there's subtlety here that I haven't quite figured out
    -- to do with how I want to branch over all the nameservers in an
    -- RRset, and then over all the A records of all those nameservers
    -- and return a failure in the case of all of them failing. So its
    -- not a pure fork but needs some join back in, perhaps a Join
    -- uniquely numbered query type or some such? Not sure...

    (GetNameserverAnswer ns) <- qpull (GetNameserverQuery domainSuffixByteString)
    report $ "Got nameserver " ++ (show ns) ++ " for domain " ++ (show domainSuffixByteString)

    -- this should be a recursive launch, I think, rather than only a pull
    (GetRRSetAnswer (Right alist)) <- qpull (GetRRSetQuery ns A)

    forA_ alist $ \arec -> do
      report $ "Nameserver " ++ (show ns) ++ " has address: " ++ (show arec)

    empty

  -- the above may not continue in the monad - it will only continue if one or more of the threads generates a non-empty value.

  return ()

-- | this should return the IP addresses the named host
resolve :: LDomain -> Q () ()
resolve domain = do
  report ("Resolving " ++ (show domain))


  -- TODO: case 1 and case {2,3,4} are probably parallel activities
  -- with the cached case falling off the end with no comment
  -- and the other ones returning potentially errors.

  {-
   rfc 1034 s5.3.3
   1. See if the answer is in local information, and if so return
      it to the client.
  -}

  c <- isCached domain

  report ("Is cached? " ++ (show c))
  when (c == True) (error "Cached case unimplemented")

  {- rfc 1034 s5.3.3
   2. Find the best servers to ask.

  In a regular resolver, this is the closest ancestor name
  servers. But in this search-everything style, I want to
  enquire of all ancestor name servers.
  -}

  -- this needs to return the name servers one by one so that
  -- each can be executed independently
  nameServer <- getAncestorNameServer domain

  report ("need to query nameserver " ++ (show nameServer) ++ " for domain " ++ (show domain))

  {-
   3. Send them queries until one returns a response.

   4. Analyze the response, either:

         a. if the response answers the question or contains a name
            error, cache the data as well as returning it back to
            the client.

         b. if the response contains a better delegation to other
            servers, cache the delegation information, and go to
            step 2.

         c. if the response shows a CNAME and that is not the
            answer itself, cache the CNAME, change the SNAME to the
            canonical name in the CNAME RR and go to step 1.

         d. if the response shows a servers failure or other
            bizarre contents, delete the server from the SLIST and
            go back to step 3. 
  -}


isCached :: LDomain -> Q any Bool
isCached _ = return False

report :: String -> Q any ()
report s = unsafeQT $ putStrLn s

getAncestorNameServer :: LDomain -> Q any LDomain
getAncestorNameServer domain = return ["a","root-servers","net"]

