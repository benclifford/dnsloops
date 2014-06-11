{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternGuards #-}

module Main where

import Control.Applicative
import Data.ByteString.Char8 (pack, split, intercalate, unpack)
import Data.Foldable (asum, Foldable)
import Data.IP
import Data.List (tails, nub)
import Data.Typeable

import Network.DNS

import System.Environment (getArgs)

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
    -- we record the answer but we also shred it up and cache
    -- other things it might be an answer to.
    (qrecord q (ResolverAnswer result))
      <|> cacheResolverAnswer d t result -- TODO: this probably needs more info about the query in order to perform validation.

-- TODO: this can hopefully supercede GetNameserver more generally.
data GetRRSetQuery = GetRRSetQuery Domain TYPE deriving (Show, Eq, Typeable)
data GetRRSetAnswer = GetRRSetAnswer (Either String [ResourceRecord]) deriving (Show, Eq, Typeable)

instance Qable GetRRSetQuery GetRRSetAnswer where
  runQable q@(GetRRSetQuery d ty) =
    (report $  "Launching complex resolve for GetRRSetQuery: " ++ (show d) ++ " " ++ (show ty))
    <|>
    complexResolve d ty
    -- TODO: do I need this empty on the end?
    -- I'm unclear in general about when returning a () vs
    -- returning empty makes sense.
    -- TODO: what to do with the results?

data GetNameserverQuery = GetNameserverQuery Domain deriving (Show, Eq, Typeable)
data GetNameserverAnswer = GetNameserverAnswer Domain deriving (Show, Eq, Typeable)
-- TODO: there should be another answer option that is NoNameserver

-- TODO: this implementation uses the local default resolver
-- rather than recursively starting something new
-- which is what should happen (probably via GetRRSetQuery)
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

  [h] <- getArgs

  let hostname = pack h

  putStrLn "============ Test 1 ============"
  res <- runQ (simpleQuery hostname)
  putStrLn "Final result in Main: "
  print res

  putStrLn "============ Test 2 ============"
  res <- runQ $ populateRootHints <|> (query $ GetRRSetQuery hostname A)

  putStrLn "Final result in Main: "
  print res


simpleQuery hostname = query (ResolverQuery defaultResolvConf hostname A)

populateRootHints = 
      (qrecord (GetNameserverQuery rootName)
               (GetNameserverAnswer aName) *> empty)
  <|> (qrecord (GetRRSetQuery aName A)
               (GetRRSetAnswer $ Right [ResourceRecord aName A 0 noLen (RD_A aIP)]) *> empty)
  where rootName = pack ""
        aName = pack "a.root-servers.net"
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

-- | for-alternative: rather than performing actions using applicative
-- sequence, perform them using alternative <|>
-- (c.f. for in Traversable (?))
forA_ :: (Foldable t, Functor t, Alternative a) => t x -> (x -> a y) -> a y
forA_ l f = asum (fmap f l)

-- | complexResolve must not add final results because
-- it is used recursively.
-- but it should record RRsets for GetRRSetQuery results
-- I think.
complexResolve :: Typeable final => Domain -> TYPE -> Q final ()
complexResolve qname qrrtype = do

  let shreddedDomain = split '.' qname 
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

    report $ "Requesting nameserver for domain " ++ (unpack domainSuffixByteString)
    (GetNameserverAnswer ns) <- qpull (GetNameserverQuery domainSuffixByteString)
    report $ "Got nameserver " ++ (show ns) ++ " for domain " ++ (show domainSuffixByteString)

    (GetRRSetAnswer (Right alist)) <- query (GetRRSetQuery ns A)

    forA_ alist $ \arec -> do
      report $ "Nameserver " ++ (show ns) ++ " has address: " ++ (show arec)
      -- TODO: check arec has rrtype A and fail a bit if not (by which I mean taint something appropriately and continue)
      let rd = show $ rdata arec
      report $ "Nameserver string is: " ++ (rd)
      -- TODO: now, query that nameserver for the whole domain
      -- and looping needs to happen by the results matching
      -- a different (deeper) branch of the above query.
      -- do this in a Qable so as to support things later like
      -- retrying of queries to check consistency, and
      -- launching queries from different network locations
      -- TODO: parameterise the A
      (ResolverAnswer r) <- query $ ResolverQuery
        (defaultResolvConf { resolvInfo = RCHostName rd })
        qname qrrtype
      report $ "complexResolve: When querying nameserver " ++ (show ns) ++ " [" ++ (rd) ++ "] for " ++ (show qname) ++ "/" ++ (show qrrtype) ++ ", a resolver result is " ++ (show r)


      case r of 
        (Right df) | (rcode . flags . header) df == NoErr
                   , answer df == []
                   , nub (rrtype <$> (authority df)) == [NS]
                   , [delegzone] <- (nub (rrname <$> (authority df)))
          ->  report $ "complexResolve: can ignore delegation"
        (Right df) | (rcode . flags . header) df == NoErr
                   , ans <- answer df
                   , ans /= []
          -> do
               report $ "complexResolve: processing answer records (NOTIMPL) " ++ (show $ ans)
               empty
               -- TODO: validation of other stuff that should or should not be here... (for example, is this an expected answer? is whatever is in additional and authority well formed?)

        _ -> (report $ "complexResolve: UNEXPECTED result from resolver: Querying for " ++ (show qname) ++ "/" ++ (show qrrtype) ++ " => " ++ (show r)) *> empty -- TODO something more interesting here

    empty

  -- the above may not continue in the monad - it will only continue if one or more of the threads generates a non-empty value.  so using do notation is maybe not the right thing to be doing here.

  return ()




cacheResolverAnswer qname qrrtype r = do
      -- so this might be our answer! (one day)
      -- or more likely its a delegation - what does that look like according
      -- to the spec? My informal understanding is we get 
      -- rcode == NoErr, answer = [] and some authority records that
      -- are for deeper servers than what we just got given.
      -- (how does that look different than, for example, a response
      -- that is saying that there are no records of that type
      -- at this label?)
      -- looks like we get an SOA for the "no records of this type here"
      -- in the authority section, and NS records in the authority section
      -- if its a delegation. (and depending on who we're asking, we might
      -- get an auth answer bit set or not - probably not if we've gone
      -- via a recursive resolver...)

      -- TODO: I think all delegation NSes should be for the same
      -- domain - there shouldn't be any delegation to different levels
      -- in the same response. Check and report.

      -- TODO: check the delegation is for somewhere between the name
      -- server we just queried and the end hostname

      -- TODO: is it appropriate to be doing this here or
      -- in the response bit of ResolverAnswer?
      -- If I'm only making resolver queries in one place, I guess
      -- it doesn't matter, but I want this cache behaviour to
      -- happen for every query, not just ones that I happen to have
      -- decided to pull for in this code block. So maybe it should
      -- move in there.

      -- TODO: this case needs splitting into functions for
      -- readability
      case r of 
        (Right df) | (rcode . flags . header) df == NoErr
                   , answer df == []
                   , nub (rrtype <$> (authority df)) == [NS]
                   , [delegzone] <- (nub (rrname <$> (authority df)))
          ->    (report $ "cacheResolverAnswer: This answer is a DELEGATION of zone " ++ (show delegzone))
              <|> (forA_ (rdata <$> (authority df)) (\rd -> 
                         (report $ "cacheResolverAnswer: DELEGATION of zone " ++ (show delegzone) ++ " to " ++ (show rd)) *>
                         (qrecord (GetNameserverQuery $ pack $ dropdot $ unpack delegzone)  -- TODO this is pretty ugly - I should stop using strings so much for passing around domains, and instead use a list of labels
                                  (GetNameserverAnswer (pack $ dropdot $ show rd)) *> empty))
                )
              <|>
                (
                 -- TODO now this is a bit weird: I don't necessary want to
                 -- go parallel for each record: I want to make up, potentially,
                 -- grouped rrsets (grouped by rrname/rrtype) and go parallel
                 -- over those. Although in practice do I ever see the same
                 -- rrname/rrtype more than once in additional data?
                 -- At least put a check in for it and give a WARNING about
                 -- unhandled behaviour
                 forA_ (additional df) (\ad -> 
                      (report $ "cacheResolverAnswer: DELEGATION ADDITIONAL DATA: " ++ (show ad))
  
                   *> (qrecord (GetRRSetQuery (pack $ dropdot $ unpack $ rrname ad) (rrtype ad))
                               (GetRRSetAnswer (Right [ad])) -- TODO: see above TODO: if I've grouped by rrname/rrtype, this list may have more than one entry
                      )
{-

-- TODO: this can hopefully supercede GetNameserver more generally.
data GetRRSetQuery = GetRRSetQuery Domain TYPE deriving (Show, Eq, Typeable)
data GetRRSetAnswer = GetRRSetAnswer (Either String [ResourceRecord]) deriving (Show, Eq, Typeable)


-}
                 *> empty
                  ) 
                 *>
                   -- TODO: we also have some additional data to validate and push into the db
                empty
               )
             *> empty
        (Right df) | (rcode . flags . header) df == NoErr
                   , ans <- answer df
                   , ans /= []
          -> do
               report $ "Processing answer records: " ++ (show $ ans)
               -- TODO: group the answers into RRSets of common name and type, rather than pushing RRSet answers as individual rows.
               forA_ ans (\rr ->
                 (report $ "cacheResolverAnswer: ANSWER (improperly formed rrset due to my bad code)" ++ show rr)
                <|>
                 (qrecord (GetRRSetQuery (pack $ dropdot $ unpack $ rrname rr) (rrtype rr)) (GetRRSetAnswer (Right [rr]))
                 ))
               empty
               -- TODO: validation of other stuff that should or should not be here... (for example, is this an expected answer? is whatever is in additional and authority well formed?)

        _ -> (report $ "cacheResolverAnswer: UNEXPECTED result from resolver: Querying for " ++ (show qname) ++ "/" ++ (show qrrtype) ++ " => " ++ (show r)) *> empty -- TODO something more interesting here







dropdot :: String -> String
dropdot s | last s == '.' = init s
dropdot s = error $  "Cannot drop the dot off a string that does not end with a dot: " ++ s

-- | this should return the IP addresses the named host
{-
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

-}

report :: String -> Q any ()
report s = unsafeQT $ putStrLn s

{-
getAncestorNameServer :: LDomain -> Q any LDomain
getAncestorNameServer domain = return ["a","root-servers","net"]
-}

-- TODO:
-- given a list of recursive resolvers, send all queries through
-- those as well as manually resolving, so as to check they
-- are not diddling. an example of this would be advert
-- inserting DNS.

-- TODO:
-- make queries from different network locations over some
-- kind of tunnel

-- TODO:
-- make queries over TCP
--   check query from TCP is same as (or indicated truncated prefix of)
--   the UDP version

-- TODO: DNSSEC

-- TODO: make fuzzy random queries around any requested domain
-- to try to deliberately get strang things into the
-- cache

-- TODO: make same query again after a delay for a bit
-- to check results are consistent

-- TODO: for each known name server IP, query it for
-- various server ID forms (the bind one and the
-- RFC one). give post-facto report on consistency
-- (can have post-facto reports build incrementally
-- but not permitted to cause new queries to run)

-- TODO: get SOA for every known zone from every known
-- server and give reports on inconsistency

-- TODO: check that master in SOA matches a nameserver
-- (is that a requirement of spec) and give report
-- when that is not the case

-- TODO: provenance check that we can get the answer
-- when only on ipv6

-- TODO: provenance check that we can get the answer
-- when only on ipv4

-- TODO: log unexpected return results

-- TODO: detect if a nameserver has no addresses (in any particular
-- "account" of a query)

-- TODO: check delegated nameservers list and nameserver list from
-- all other name servers is consistent.

-- TODO: glue validation: check each glue RR against authoritative
-- record
-- check if glue is actually needed (and error if not)
-- check glue is for the relevant servers (that is, for servers
-- in the auth section)

-- TODO: deal with truncation

-- TODO: rfc2181 s5 : check if any RRSet contains duplicates and give warning

-- TODO: rfc2181 s5.2: different TTLs for records in RRset is deprecated.
--    check for that and warn (actually 5.2 says it should be treated as
--    an error, so then I should make that potentially stop resolving, as
--    well as continuing)

-- TODO: stack depth/work performed timeout counter to stop
-- infinte loops, or some proof that infinite loops will not
-- happen even in the presence of weird browser queries
-- (which I think is not true, because I can always say
-- zone n is served by servers in zone n+1 and then cause
-- a further query for n+1 name servers.

-- TODO: make sure no use of recursive/system resolver except
-- when doing explicit lookaside.

-- TODO: examine SOA fields: validate SMTP is listening on the listening port? validate that the SOA master field is a nameserver in the NS records. Potentially use that SOA master as another nameserver if it is not so described, which could generate a low-priority warning. (esp if it reveals inconsistencies) - that bit is more linty rather than actual errors.

-- TODO: examine authoritative servers and see if they advertise recursion available. linty-warning if they do.

-- TODO: know when we requested recursion or not, and how that interacts with authoritative data or not, and store that in provenance somehow.

-- TODO: detect mutual no-glue loops (and give warning?) - where two zones use each other at least in part to serve each other, so there is no glue but they are at least partially mutually dependent for each others resolvability.

-- TODO: for every PTR and A record encountered, validate that they have correct forward/backward lookup - this can be a warning if not matching, but some times (eg on MX?) spam software will not like it if theres a mismatch, so warn in that case harder.

-- TODO: warn on CNAMEs where a CNAME shouldn't be (NS and MX? rfc cites?)

