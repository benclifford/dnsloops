{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.ByteString.Char8 (pack)
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

-- | these instances are needed to support Show and Eq on ResolverQuery and ResolverAnswer
deriving instance Show ResolvConf
deriving instance Eq ResolvConf
deriving instance Show FileOrNumericHost
deriving instance Eq FileOrNumericHost

hostname = pack "www.hawaga.org.uk"

main = do
  putStrLn "DNSLoops main"

{-
  resolver <- makeResolvSeed defaultResolvConf
  result <- withResolver resolver $ \r -> lookupRaw r hostname A
  print result 
-}

--  runQ $ resolve ["www", "hawaga", "org", "uk"]

  res <- runQ simpleQuery

  putStrLn "Final result in Main: "
  print res


simpleQuery = query (ResolverQuery defaultResolvConf hostname A)




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

