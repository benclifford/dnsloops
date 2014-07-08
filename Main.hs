{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (pack, unpack)
import Data.IP
import Data.List (tails, nub, groupBy, sortBy)
import Data.Typeable

import Network.DNS

import System.Environment (getArgs)
import System.IO.Error

import Domain
import Instances
import Q
import Util
import Control.Monad

data ResolverQuery = ResolverQuery ResolvConf Domain TYPE deriving (Show, Eq, Typeable)
data ResolverAnswer =
    ResolverAnswer (Either ResolverError DNSFormat)
  deriving (Show, Eq, Typeable)

data ResolverError =
    ResolverDNSError DNSError
  | ResolverIOError IOError
  deriving (Show, Eq, Typeable)

instance Qable ResolverQuery ResolverAnswer where
  runQable q@(ResolverQuery rc d t) = do
    result <- liftIO $ do
      resolver <- makeResolvSeed rc
      tryIOError $ withResolver resolver $ \r -> lookupRaw r d t
    -- we record the answer but we also shred it up and cache
    -- other things it might be an answer to.
    case result of
      Right res ->
        (qrecord q (ResolverAnswer (liftDNSError res)))
        <|> cacheResolverAnswer rc d t res -- TODO: this probably needs more info about the query in order to perform validation.
      Left ioErr -> qrecord q (ResolverAnswer (Left $ ResolverIOError ioErr))
        -- TODO: need to cache IOErrors somehow. In the same way as I'm recording DNSError I guess...

liftDNSError :: Either DNSError a -> Either ResolverError a
liftDNSError (Left e) = Left (ResolverDNSError e)
liftDNSError (Right v) = Right v

-- TODO: this can hopefully supercede GetNameserver more generally.
data GetRRSetQuery = GetRRSetQuery Domain TYPE deriving (Show, Eq, Typeable)
data GetRRSetAnswer = GetRRSetAnswer (Either String CRRSet) deriving (Show, Eq, Typeable)

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

instance Qable GetNameserverQuery GetNameserverAnswer where
  runQable q@(GetNameserverQuery d) = do
    GetRRSetAnswer a <- query $ GetRRSetQuery d NS
    case a of
      Left e -> report $ "Unhandled: error when getting nameserver for domain " ++ (show d)
      -- TODO ^ what to record for the error case?
      Right (CRRSet rrs) -> forA_ rrs $ \rr -> let
        (RD_NS nsrv) = rdata rr
        in qrecord q (GetNameserverAnswer nsrv)


main = do
  putStrLn "DNSLoops main"

  [h, tys] <- getArgs

  let hostname = pack h
  let ty = read tys

  (res, db) <- runQ $ populateRootHints <|> (query $ GetRRSetQuery hostname ty)

  putStrLn "Database stats:"
  putStrLn $ "Number of previous launches: " ++ (show $ length $ previousLaunches db)
  putStrLn $ "Number of previous results: " ++ (show $ length $ previousResults db)
  putStrLn $ "Number of previous pulls: " ++ (show $ length $ previousPulls db)
  putStrLn $ "Number of final results: " ++ (show $ length $ finalResults db)

  let launchTypes = typeOfPreviousLaunch <$> ((previousLaunches db))
        where typeOfPreviousLaunch (PreviousLaunch q) = typeOf q

  putStrLn $ "Launch types: " ++ (show $ nub launchTypes)
  forM_ (nub launchTypes) $ \lt -> do
    putStr "  "
    putStr (show lt)
    putStr ": "
    putStr $ show $ length $ filter (== lt) $ launchTypes
    putStrLn ""

  -- we could get the type of a here, but there is slightly
  -- more information contained in q because multiple
  -- q types can share the same a type.
  let resultTypes = typeOfPreviousResult <$> ((previousResults db))
        where typeOfPreviousResult (PreviousResult q a) = typeOf q

  putStrLn $ "Result types: " ++ (show $ nub resultTypes)
  forM_ (nub resultTypes) $ \lt -> do
    putStr "  "
    putStr (show lt)
    putStr ": "
    putStr $ show $ length $ filter (== lt) $ resultTypes
    putStrLn ""

  putStrLn "Final result in Main: "
  print `mapM` res

typeOfPreviousLaunch (PreviousLaunch q) = typeOf q

populateRootHints = 
      (qrecord (GetNameserverQuery rootName)
               (GetNameserverAnswer aName) *> empty)
  <|> (qrecord (GetRRSetQuery aName A)
               (GetRRSetAnswer $ Right $ canonicaliseRRSet $ [ResourceRecord aName A 0 noLen (RD_A aIP)]) *> empty)
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

-- | complexResolve must not add final results because
-- it is used recursively.
-- but it should record RRsets for GetRRSetQuery results
-- I think.
complexResolve :: Typeable final => Domain -> TYPE -> Q final ()
complexResolve qname qrrtype = do

  let domainSuffixes = ancestorDomains qname

  forA_ domainSuffixes $ \domainSuffixByteString -> do

    -- there's subtlety here that I haven't quite figured out
    -- to do with how I want to branch over all the nameservers in an
    -- RRset, and then over all the A records of all those nameservers
    -- and return a failure in the case of all of them failing. So its
    -- not a pure fork but needs some join back in, perhaps a Join
    -- uniquely numbered query type or some such? Not sure...

    report $ "Requesting nameserver for domain " ++ (unpack domainSuffixByteString)
    (GetNameserverAnswer ns) <- query (GetNameserverQuery domainSuffixByteString)
    report $ "Got nameserver " ++ (show ns) ++ " for domain " ++ (show domainSuffixByteString)

    -- TODO BUG: this pattern match will fail if the query
    -- returns a failure. That query should perhaps cascade,
    -- or generate a warning, or what? Probably need to think
    -- about what happens in the normal algorithm when this
    -- fails.
    -- (GetRRSetAnswer (Right alist)) <- query (GetRRSetQuery ns A)
    nserverAddressRRSet <- query (GetRRSetQuery ns A)
    case nserverAddressRRSet of 
      (GetRRSetAnswer (Left e)) ->
        qrecord (GetRRSetQuery qname qrrtype) (GetRRSetAnswer (Left $ "Could not get address records for nameserver "++(show ns)))
      (GetRRSetAnswer (Right (CRRSet aRRset))) -> do

        forA_ aRRset $ \arec -> do
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
          debugReport $ "complexResolve: When querying nameserver " ++ (show ns) ++ " [" ++ (rd) ++ "] for " ++ (show qname) ++ "/" ++ (show qrrtype) ++ ", a resolver result is " ++ (show r)

        empty

  -- the above may not continue in the monad - it will only continue if one or more of the threads generates a non-empty value.  so using do notation is maybe not the right thing to be doing here.

  return ()




cacheResolverAnswer server qname qrrtype r = do
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
      -- readability. Perhaps can use applicative parser style
      -- to match various cases in sequence, as named cases,
      -- rather than using a case construct.
      case r of 
        (Right df) | (rcode . flags . header) df == NoErr
                   , answer df == []
                   , nub (rrtype <$> (authority df)) == [NS]
                   , [delegzone] <- (nub (rrname <$> (authority df)))
          ->    (report $ "cacheResolverAnswer: This answer is a DELEGATION of zone " ++ (show delegzone))
              <|> (forA_ (rdata <$> (authority df)) (\rd -> 
                         (report $ "cacheResolverAnswer: DELEGATION of zone " ++ (show delegzone) ++ " to " ++ (show rd)) *>
                         (qrecord (GetNameserverQuery $ dropDot $ delegzone)  -- TODO this is pretty ugly - I should stop using strings so much for passing around domains, and instead use a list of labels
                                  (GetNameserverAnswer (dropDot $ rdataNSToDomain rd)) *> empty))
                )
              <|> (recordRRlist (authority df) *> empty)
              <|> (recordRRlist (additional df) *> empty)
{-

-- TODO: this can hopefully supercede GetNameserver more generally.
data GetRRSetQuery = GetRRSetQuery Domain TYPE deriving (Show, Eq, Typeable)
data GetRRSetAnswer = GetRRSetAnswer (Either String [ResourceRecord]) deriving (Show, Eq, Typeable)


-}
-- TODO: BUG: this is misfiring when receiving some CNAME redirections
-- rather than passing on to the CNAME case.
        (Right df) | (rcode . flags . header) df == NoErr
                   , ans <- answer df
                   , ans /= []
                   , [qrrname] <- (nub (rrname <$> ans))
                   , [qrrtype] <- (nub (rrtype <$> ans))
                      -- check we only got answer records for the
                      -- rname and type that we requested
          -> 
                (report $ "Processing answer records: " ++ (show $ ans))
            <|> (recordRRlist ans *> empty)
            <|> (recordRRlist (authority df) *> empty)
            <|> (recordRRlist (additional df) *> empty)
               -- TODO: validation of other stuff that should or should not be here... (for example, is this an expected answer? is whatever is in additional and authority well formed?)
               -- TODO: cache any additional data that we got here

        -- The CNAME case:
        -- If we didn't request a cname, but the answer
        -- contains a CNAME, and potentially other
        -- answers, including potentially more CNAMEs.
        (Right df) | (rcode . flags . header) df == NoErr
                   , qrrtype /= CNAME
                   , ans <- answer df
                   , ans /= []
                   , CNAME `elem` (rrtype <$> ans)
                   -- there should be one unique record that
                   -- is a CNAME for the 
                      -- check we only got answer records for the
                      -- rname and type that we requested
          -> 
               -- TODO:
               -- we need to record the answers,authority,additional
               -- but we need to also launch a new lookup of the
               -- CNAME and transpose any results from that into
               -- results for this lookup.
                (recordRRlist ans *> empty) -- so we'll have both the cname and the transposed answers recorded
            <|> (recordRRlist (authority df) *> empty)
            <|> (recordRRlist (additional df) *> empty)
            <|> (report $ "Following CNAME: "++(show ans))
            <|>
               ( do
                    let cql = rdata <$>
                                     (filter (\rr -> rrname rr =.= qname
                                                  && rrtype rr == CNAME)
                                             ans)
                    -- liftIO $ error $ "cql = " ++ (show cql) ++ ", ans = " ++ (show ans) ++ ", qname = " ++ (show qname)
                    let [RD_CNAME cqname] = cql
                    -- liftIO $ error $ "cqname to resolve: " ++ (show cqname)
                    cnamedRRSet <- query (GetRRSetQuery (dropDot cqname) qrrtype)
                    -- this will either be a Left or a Right
                    -- if its a Right, transpose that RRSet
                    -- if its a Left, make a stack-trace like nested Left
                    case cnamedRRSet of
                      GetRRSetAnswer (Left err) ->
                        qrecord (GetRRSetQuery qname qrrtype) (GetRRSetAnswer $ Left $ "When resolving CNAME: " ++ err) *> empty
                      GetRRSetAnswer a@(Right _) ->
                        qrecord (GetRRSetQuery qname qrrtype) (GetRRSetAnswer a) *> empty
                )

        -- Two kinds of name not found: one from the name not existing, and
        -- one for the name existing but not having values of the
        -- requested rrtype.
        
        (Right df) | (rcode . flags . header) df == NameErr
                   , answer df == []
                   , additional df == []
                   , nub (rrtype <$> (authority df)) == [SOA]
          -> qrecord (GetRRSetQuery qname qrrtype) (GetRRSetAnswer (Left "Name does not exist, RCODE 3"))

        (Right df) | (rcode . flags . header) df == NoErr
                   , answer df == []
                   , nub (rrtype <$> (authority df)) == [SOA]
          -> qrecord (GetRRSetQuery qname qrrtype) (GetRRSetAnswer (Left "Name has no rrdata of this rrtype"))

        _ -> do
               report $ "cacheResolverAnswer: UNEXPECTED result from resolver: Querying for " ++ (show qname) ++ "/" ++ (show qrrtype) ++ " => " ++ (show r)
               qrecord (GetRRSetQuery qname qrrtype) (GetRRSetAnswer (Left $ "Unexpected result: when querying nameserver " ++ (show $ resolvInfo server)))
               empty -- TODO something more interesting here
               -- TODO: i wonder how unexpected results should propagate when used to lookup values used already? I guess I want to look at the results for an RRset as a whole to see if all entries return an unexpected results rather than eg at least one returning an OK result - so that we can generate different warn levels.


recordRRlist rs = let
  rrsets = rrlistToRRsets rs
  in forA_ rrsets (\rrset ->
          (report $ "cacheResolverAnswer: ANSWER RRset: " ++ show rrset)
          <|>
          (
             let
               exampleRR = head rrset
               in (qrecord (GetRRSetQuery (dropDot $ rrname exampleRR) (rrtype exampleRR)) (GetRRSetAnswer (Right $ canonicaliseRRSet rrset)))
          )
      )


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

-- TODO: this internet-draft: 
-- http://tools.ietf.org/html/draft-koch-dns-glue-clarifications-03
-- might have some interesting stuff about glue.
-- also RFC4472

-- TODO: detect private IPv4 and link/site scoped IPv6 addresses in A/AAAA records

-- TODO: generate warning when some-but-not-all servers in an NS RRset error.

-- TODO: examine eg pool.ntp.org as an example of how I should get some warnings when i) different servers are returning different result sets; and ii) the same server queried repeatedly gives out different results.

-- TODO: an actual problem that occurred with A&A: one of the
-- auth.primary-dns.co.uk was timeing out. the other was answering
-- so I want to know that the result was aquired but I also want
-- some kind of warning saying a timeout occurred on the way to
-- this result. What is the right UI for presenting this info?
-- remember that it may happen anywhere in the Q run, not just
-- in the direct parents of the current query.

-- TODO: case of authoritative denial of existence:
-- no answer, and SOA records in the authority section

-- TODO: optional check of all known "auth" NSes for:
--   i) recursive resolving
--  ii) root referrals
--  and generate warnings of such

-- TODO: when querying a root server for a single label, eg "net", an InvalidDomain error
-- is returned  - perhaps this needs to be terminated with a .?

-- TODO: make an ANY query against any name that has been requested, so as to check it is returned and that it is the same.

-- TODO: some kind of balliwick awareness a la http://homepage.ntlworld.com/jonathan.deboynepollard/FGA/dns-server-bailiwick.html

-- TODO: detect cache-poisoning (perhaps overlapping with bailiwick handling?) djb has interesting stuff here http://cr.yp.to/djbdns/notes.html

-- TODO:  according to djb doc abouve, "RFC 1034 says that an alias ``should'' not point to another alias."
-- Detect this and issue a warning

-- TODO: detect double/multiple CNAME chain and report warning - somewhere says CNAMEs should not point to CNAMEs.

