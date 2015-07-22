{-# LANGUAGE OverloadedStrings, FlexibleContexts, GeneralizedNewtypeDeriving #-}

module APIUpdater where

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

import Aws
  ( Response
  , ResponseMetadata
  , aws
  , baseConfiguration
  , readResponse
  )

import Control.Monad.Except
  ( ExceptT (ExceptT)
  , MonadError
  , runExceptT
  , throwError
  )

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )

import Data.Foldable (find)

import Control.Applicative (Applicative, pure, (<$>), (<*>))

import Network.HTTP.Conduit (withManager)

newtype APIUpdater a = APIUpdater { unAPIUpdater :: ExceptT APIError IO a }
  deriving ( Functor, Applicative, Monad
           , MonadError APIError, MonadIO)

runAPIUpdater :: APIUpdater a -> IO (Either APIError a)
runAPIUpdater = runExceptT . unAPIUpdater

data APIError = FindZoneError
              | UpdateError
              deriving (Show)

updateDns :: Domain -> Domain -> ResourceRecord
          -> APIUpdater (Response Route53Metadata ChangeResourceRecordSetsResponse)
updateDns tZone domain ip = do
  zone <- findZoneId tZone =<< getHostedZones
  updateRecordSet
    (hzId zone)
    domain
    ip

findZoneId :: (MonadIO m, MonadError APIError m)
           => Domain -> HostedZones -> m HostedZone
findZoneId domain zones =
  case find (\zone -> hzName zone == domain) zones of
       Nothing -> throwError FindZoneError
       Just zone -> return zone

getHostedZones :: MonadIO m
               => m HostedZones
getHostedZones = liftIO $ do
  cfg <- baseConfiguration
  resp <- withManager $ \mgr ->
    aws cfg route53 mgr listHostedZones
  ListHostedZonesResponse { lhzrHostedZones = zones } <- readResponse resp
  return zones

updateRecordSet :: (MonadIO m, MonadError APIError m)
                => HostedZoneId -> Domain -> ResourceRecord
                -> m (Response
                        (ResponseMetadata ChangeResourceRecordSetsResponse)
                        ChangeResourceRecordSetsResponse)
updateRecordSet zoneId domain ip = liftIO $ do
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
  cfg <- baseConfiguration
  withManager $ \mgr ->
    aws cfg route53 mgr $ ChangeResourceRecordSets zoneId comment changes
