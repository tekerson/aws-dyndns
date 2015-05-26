{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )

import Aws
  ( Response
  , ResponseMetadata
  , aws
  , baseConfiguration
  , readResponse
  )

import Aws.Route53
  ( ACTION (UPSERT)
  , ChangeResourceRecordSets (ChangeResourceRecordSets)
  , ChangeResourceRecordSetsResponse (ChangeResourceRecordSetsResponse)
  , Domain (Domain)
  , HostedZone (HostedZone)
  , HostedZoneId (HostedZoneId)
  , HostedZones (..)
  , ListHostedZonesResponse (ListHostedZonesResponse)
  , RecordType (A)
  , ResourceRecord (ResourceRecord)
  , ResourceRecordSet (ResourceRecordSet)
  , Route53Metadata (Route53Metadata)
  , hzId
  , hzName
  , lhzrHostedZones
  , listHostedZones
  , route53
  , rrsAliasTarget
  , rrsName
  , rrsRecords
  , rrsRegion
  , rrsSetIdentifier
  , rrsTTL
  , rrsType
  , rrsWeight
  )

import Network.HTTP.Conduit (withManager)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void)
import Data.Foldable (find)
import Data.Text (Text, pack, unpack)

import System.Environment (getArgs)

main :: IO ()
main = do
    (zone, domain) <- parseArgs =<< getArgs
    ip <- ipAddress
    putStrLn $ "Updating to: " ++ unpack ip ++ "..."
    let zone' = Domain zone
        domain' = Domain domain
        ip' = ResourceRecord ip
    void $ updateDns zone' domain' ip'
    putStrLn "Done"

parseArgs :: (Monad m) => [String] -> m (Text, Text)
parseArgs argv = case argv of
                      [zone, domain] -> return (pack zone, pack domain)
                      _ -> fail usage

ipAddress :: IO Text
ipAddress = pack <$> (simpleHTTP (getRequest "http://ipecho.net/plain") >>= getResponseBody)

usage :: String
usage = "Usage: Zone Domain Ip"

convert :: (Text, Text) -> (Domain, Domain)
convert (zone, domain) = (Domain zone, Domain domain)

updateDns :: Domain -> Domain -> ResourceRecord
          -> IO (Response Route53Metadata ChangeResourceRecordSetsResponse)
updateDns tZone domain ip = do
  zone <- findZoneId tZone =<< getHostedZones
  liftIO $ updateRecordSet
    (hzId zone)
    domain
    ip

findZoneId :: MonadIO m => Domain -> HostedZones -> m HostedZone
findZoneId domain zones =
  case find (\zone -> hzName zone == domain) zones of
       Nothing -> fail "Not Found"
       Just zone -> return zone

getHostedZones :: IO HostedZones
getHostedZones = do
  cfg <- Aws.baseConfiguration
  resp <- withManager $ \mgr ->
    Aws.aws cfg route53 mgr listHostedZones
  ListHostedZonesResponse { lhzrHostedZones = zones } <- readResponse resp
  return zones

updateRecordSet ::
  HostedZoneId -> Domain -> ResourceRecord
  -> IO (Response
          (ResponseMetadata ChangeResourceRecordSetsResponse)
          ChangeResourceRecordSetsResponse)
updateRecordSet zoneId domain ip = do
  let
    comment = Just "Set by: aws-dyndns"
    record = ResourceRecordSet
      { rrsName = domain
      , rrsType = A
      , rrsAliasTarget = Nothing
      , rrsSetIdentifier = Nothing
      , rrsWeight = Nothing
      , rrsRegion = Nothing
      , rrsTTL = Just 60
      , rrsRecords = [ ip ]
      }
    changes = [(UPSERT, record)]
  cfg <- Aws.baseConfiguration
  withManager $ \mgr ->
    Aws.aws cfg route53 mgr $ ChangeResourceRecordSets zoneId comment changes
