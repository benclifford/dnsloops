{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

module Main where

import Control.Applicative
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader(ReaderT())
import Data.ByteString.Char8 (pack, unpack)
import Data.IP
import Data.List (nub)
import Data.Typeable

import Network.DNS

import System.IO.Error

import Domain
import Instances()
import Q
import Query.Resolver
import Util


-- There are two stages to processing:
-- * the dynamic stage happens in the Q monad, and so code
--   (eg checking rules) can launch new queries and cause
--   new answers to appear. Because of this, some checks
--   (such as processing over the entire list of results
--   of a query) cannot be performed.
-- * the static stage happens in the IO monad, or functionally,
--   with read-only access to the query database. Processing
--   performed at this stage cannot launch new queries and
--   so no new answers will appear. This means we can run
--   queries such as examining the entire list of results
--   of a query.
type DynamicStage = Q GetRRSetAnswer GetRRSetAnswer
type StaticStage = ReaderT (DB GetRRSetAnswer) IO ()

instance Qable ResolverQuery where
  type Answer ResolverQuery = ResolverAnswer
  runQable q@(ResolverQuery rc d t) = do
    qcontext $ "Resolver query: " ++ (show q)
    result <- liftIO $ do
      resolver <- makeResolvSeed rc
      tryIOError $ withResolver resolver $ \r -> lookupRaw r d t
    -- we record the answer but we also shred it up and cache
    -- other things it might be an answer to.
    case result of
      Right res ->
        (qrecord q (ResolverAnswer (liftDNSError res)))
        <|> cacheResolverAnswer d t res -- TODO: this probably needs more info about the query in order to perform validation.
      Left ioErr -> qrecord q (ResolverAnswer (Left $ ResolverIOError ioErr))
        -- TODO: need to cache IOErrors somehow. In the same way as I'm recording DNSError I guess...

liftDNSError :: Either DNSError a -> Either ResolverError a
liftDNSError (Left e) = Left (ResolverDNSError e)
liftDNSError (Right v) = Right v


data GetRRSetQuery = GetRRSetQuery Domain TYPE deriving (Show, Eq, Typeable, Ord)
data GetRRSetAnswer = GetRRSetAnswer (Either String CRRSet) deriving (Show, Eq, Typeable)
instance Qable GetRRSetQuery where
  type Answer GetRRSetQuery = GetRRSetAnswer
  runQable (GetRRSetQuery d ty) = resolveRRSet d ty

populateRootHints :: DynamicStage
populateRootHints = qcontext "root hints" >>
  (
      (qrecord (GetRRSetQuery rootName NS)
               (GetRRSetAnswer $ Right $ canonicaliseRRSet $ [ResourceRecord rootName NS 0 noLen (RD_NS aName)]) *> empty)
  <|> (qrecord (GetRRSetQuery aName A)
               (GetRRSetAnswer $ Right $ canonicaliseRRSet $ [ResourceRecord aName A 0 noLen (RD_A aIP)]) *> empty)
  )
  where rootName = pack ""
        aName = pack "a.root-servers.net."
        aIP = toIPv4 [198,41,0,4]
        noLen = -1

-- I wonder if qrecord should end empty/mzero rather than returning a single () ?
-- Its probably nice to be able to use it sequentially in a do block though?

-- | resolveRRSet must not add final results because
-- it is used recursively.
-- but it should record RRsets for GetRRSetQuery results
-- I think.
resolveRRSet :: Typeable final => Domain -> TYPE -> Q final ()
resolveRRSet qName qrrtype = do
  report $  "Resolving query: " ++ (show qName) ++ "/" ++ (show qrrtype)

  let domainSuffixes = ancestorDomains qName

  void $ forA_ domainSuffixes $ \domainSuffixByteString -> do

    -- there's subtlety here that I haven't quite figured out
    -- to do with how I want to branch over all the nameservers in an
    -- RRset, and then over all the A records of all those nameservers
    -- and return a failure in the case of all of them failing. So its
    -- not a pure fork but needs some join back in, perhaps a Join
    -- uniquely numbered query type or some such? Not sure...

    report $ "Requesting nameserver for domain " ++ (unpack domainSuffixByteString)
    nses <- getNameServers domainSuffixByteString


-- TODO: what to do with the answers? The forA_s put together the results of
-- all the resolver answers at two levels, irrespective of whether they came
-- from the "same" resolver or not.
-- I guess I want to do the qrecord failure if there are no results at all
-- but I don't get to know that 'till the end. hm.
-- so maybe reporting a failure here is the wrong thing to do?
-- and perhaps I should be doing it as post-run analysis of what happened?
-- or maybe only certain failures (such as declaring non-existence, rather
-- than a timeout) should be reported as a failure.
-- So server interaction failure is different from non-existence failure,
-- which is a comment about the state of the data model
    void $ forA_ nses $ \ns -> do
     report $ "Got nameserver " ++ (show ns) ++ " for domain " ++ (show domainSuffixByteString)
     -- TODO BUG: this pattern match will fail if the query
     -- returns a failure. That query should perhaps cascade,
     -- or generate a warning, or what? Probably need to think
     -- about what happens in the normal algorithm when this
     -- fails.
     -- (GetRRSetAnswer (Right alist)) <- query (GetRRSetQuery ns A)
     nserverAddressRRSet <- query (GetRRSetQuery ns A)
     void $ case nserverAddressRRSet of 
      (GetRRSetAnswer (Left _)) -> do
        -- TODO: see above:   qrecord (GetRRSetQuery qname qrrtype) (GetRRSetAnswer (Left $ "Could not get address records for nameserver "++(show ns)))
        empty
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
            qName qrrtype
          debugReport $ "resolveRRSet: When querying nameserver " ++ (show ns) ++ " [" ++ (rd) ++ "] for " ++ (show qName) ++ "/" ++ (show qrrtype) ++ ", a resolver result is " ++ (show r)

        empty
     empty
    empty
  empty

getNameServers :: Typeable final => Domain -> Q final [Domain]
getNameServers domain = do
  GetRRSetAnswer a <- query (GetRRSetQuery domain NS)
  case a of
    Left _ ->    (report $ "Unhandled: error when getting nameserver for domain " ++ (show domain))
              *> empty
    -- TODO ^ what to record for the error case? at present, we don't return
    -- anything tied to the query, not even an error
    Right (CRRSet rrs) -> return (   ( (\(RD_NS nsrv) -> nsrv)  . rdata ) <$> rrs)


cacheResolverAnswer :: (Show err, Typeable final) => Domain -> TYPE -> Either err (DNSMessage RDATA) -> Q final ()
cacheResolverAnswer qName qrrtype r = do
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
                         (report $ "cacheResolverAnswer: DELEGATION of zone " ++ (show delegzone) ++ " to " ++ (show rd)) *> empty
                                  )
                )
              <|> (recordRRlist (authority df) *> empty)
              <|> (recordRRlist (additional df) *> empty)


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
                                     (filter (\rr -> rrname rr =.= qName
                                                  && rrtype rr == CNAME)
                                             ans)
                    -- liftIO $ error $ "cql = " ++ (show cql) ++ ", ans = " ++ (show ans) ++ ", qname = " ++ (show qname)
                    let [RD_CNAME cqname] = cql
                    -- liftIO $ error $ "cqname to resolve: " ++ (show cqname)
                    cnamedRRSet <- query (GetRRSetQuery cqname qrrtype)
                    -- this will either be a Left or a Right
                    -- if its a Right, transpose that RRSet
                    -- if its a Left, make a stack-trace like nested Left
                    case cnamedRRSet of
                      GetRRSetAnswer (Left err) ->
                        qrecord (GetRRSetQuery qName qrrtype) (GetRRSetAnswer $ Left $ "When resolving CNAME: " ++ err) *> empty
                      GetRRSetAnswer a@(Right _) ->
                        qrecord (GetRRSetQuery qName qrrtype) (GetRRSetAnswer a) *> empty
                )

        (Right df) | (rcode . flags . header) df == NoErr
                   , ans <- answer df
                   , ans /= []
                   , [_] <- (nub (rrname <$> ans))
                   , [_] <- (nub (rrtype <$> ans))
                      -- check we only got answer records for the
                      -- rname and type that we requested
          -> 
                (report $ "Processing answer records: " ++ (show $ ans))
            <|> (recordRRlist ans *> empty)
            <|> (recordRRlist (authority df) *> empty)
            <|> (recordRRlist (additional df) *> empty)
               -- TODO: validation of other stuff that should or should not be here... (for example, is this an expected answer? is whatever is in additional and authority well formed?)
               -- TODO: cache any additional data that we got here

        -- Two kinds of name not found: one from the name not existing, and
        -- one for the name existing but not having values of the
        -- requested rrtype.
        
        (Right df) | (rcode . flags . header) df == NameErr
                   , answer df == []
                   , additional df == []
                   , nub (rrtype <$> (authority df)) == [SOA]
          -> qrecord (GetRRSetQuery qName qrrtype) (GetRRSetAnswer (Left "Name does not exist, RCODE 3"))

        (Right df) | (rcode . flags . header) df == NoErr
                   , answer df == []
                   , nub (rrtype <$> (authority df)) == [SOA]
          -> qrecord (GetRRSetQuery qName qrrtype) (GetRRSetAnswer (Left "Name has no rrdata of this rrtype"))

        _ -> do
               report $ "cacheResolverAnswer: UNEXPECTED result from resolver: Querying for " ++ (show qName) ++ "/" ++ (show qrrtype) ++ " => " ++ (show r)
               qrecord (GetRRSetQuery qName qrrtype) (GetRRSetAnswer (Left $ "Unexpected result: " ++ (show r)))
               empty -- TODO something more interesting here
               -- TODO: i wonder how unexpected results should propagate when used to lookup values used already? I guess I want to look at the results for an RRset as a whole to see if all entries return an unexpected results rather than eg at least one returning an OK result - so that we can generate different warn levels.


recordRRlist :: Typeable final => [ResourceRecord] -> Q final ()
recordRRlist rs = let
  rrsets = rrlistToRRsets rs
  in forA_ rrsets (\rrset ->
          (report $ "recordRRlist: ANSWER RRset: " ++ show rrset)
          <|>
          (
             let
               exampleRR = head rrset
               in (qrecord (GetRRSetQuery (rrname exampleRR) (rrtype exampleRR)) (GetRRSetAnswer (Right $ canonicaliseRRSet rrset)))
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
-- Also can switch on RFC5001 NSID DNS option in
-- regular queries (that's a thing like DNSSEC and
-- TCP thatcould be enabled and disabled for all queries
-- and so done several times in various combinations)

-- TODO: get SOA for every known zone from every known
-- server and give reports on inconsistency

-- TODO: check that master in SOA matches a nameserver
-- (is that a requirement of spec) and give report
-- when that is not the case. If the SOA nameserver
-- is not in the nameserver RRsets, push it out as an
-- additional RRset of one element to cause queries
-- to run against it (triggered by a command line
-- option?)

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

-- TODO: make an ANY query against any name that has been requested, so as to check it is returned and that (the appropriate subset) is returned. (If not, the relevant subset should be injected as an RRset)

-- TODO: some kind of balliwick awareness a la http://homepage.ntlworld.com/jonathan.deboynepollard/FGA/dns-server-bailiwick.html

-- TODO: detect cache-poisoning (perhaps overlapping with bailiwick handling?) djb has interesting stuff here http://cr.yp.to/djbdns/notes.html

-- TODO:  according to djb doc abouve, "RFC 1034 says that an alias ``should'' not point to another alias."
-- Detect this and issue a warning

-- TODO: detect double/multiple CNAME chain and report warning - somewhere says CNAMEs should not point to CNAMEs. (see RFC 2181 s10.1)

-- TODO: some kind of "progress from root" check that checks that we could have resolved the answer(s) without needing weird caching to already be in place; warning if this is not the case

-- TODO: multicast DNS resolving. also note 13:13  * David-T discovers multicast DNS (rfc6762) uses UTF-8 not punycode
--  this could probably fit the same interface as using a direct recursive resolver. and perhaps anything nsswitchy eg YP/NIS

-- TODO: for composition of reified queries, which seems awkward without explicitly defining the composition as its own re-ified query data type, is there a category-theory way of doing this, where using >>> category composition causes a re-ified value to come into existence of some "Compose" type without needing to define the reifications explicitly? I look at this for perhaps being able to separate out normalisation of results from performing the query to acquire the results so that the database and analysis can see both?

-- TODO: when using DNSSEC, check that every single query made was authenticated with DNSSEC, so there is not some obscure path through which a non-DNSSEC attack might be made. The output of this could be reported in the form of a potential attack path of which servers in sequence we need to trick.

-- TODO: everything coming from a manually specified recursive resolver should be the same RRset as from real, except that the TTL is allowed to be smaller (but not bigger - a bigger one is an error condition, though that may have come from recently reducing the ttls on the master - it is still important to know it is happening). A similar but different subset relationship to glue checking: glue checking is allowed to miss out some records (or at least generate a lesser warning in that situation)

-- TODO: check for refused messages from a server we believe to be authoritative and that should be answering (is that every server we contact? or is there speculative stuff?)

-- TODO BUG: when network is down, I get errors like the below which are fatal to the program rather than being caught and handled as data. Although arguably they should be reported as "local system errors" rather than remote system errors, I think.
{-
Launching query ResolverQuery (ResolvConf {resolvInfo = RCHostName "192.5.6.30", resolvTimeout = 3000000, resolvRetry = 3, resolvBufsize = 512}) "B.ROOT-SERVERS.NET." A
dnsloops: getAddrInfo: does not exist (Address family for hostname not supported)
-}

-- TODO: nameservers not in zone with glue - this requires extra lookup and increases attack surface. low pri warning for this. djb reference for this I think?

-- TODO: check authoritatitive nameservers refuse recursion. this is a low-pri warning.

-- TODO: check nameservers do not return a root delegation (unless they are root servers themselves?) -- DDOS amplifier. low-pri warning.

-- TODO: reverse DNS on MX records

-- TODO: check out https://howdns.works/ webcomic and see if it has anything expository

-- TODO: case fuzzing - when we launch a query, launch the same query with fuzzed case, n-times. https://tools.ietf.org/id/draft-vixie-dnsext-dns0x20-00.txt advocated doing this for query identity, but isn't used much. it is interesting to check that servers return the same result, though.

-- TODO: ATLAS probe can be used to launch one off measurements elsewhere on the internet and observe the results.


