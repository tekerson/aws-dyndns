{-# LANGUAGE OverloadedStrings, FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Main where

import Safe (headMay)

import qualified Data.ByteString as BS

import Data.IP (IPv4)
import Network.DNS.Types (DNSError)
import Network.DNS.Lookup (lookupA)
import Network.DNS.Resolver
  ( FileOrNumericHost (RCHostName)
  , ResolvConf (ResolvConf)
  , defaultResolvConf
  , makeResolvSeed
  , resolvInfo
  , withResolver
  )

import Control.Monad.Trans.Reader

import Control.Monad.Except
  ( ExceptT (ExceptT)
  , Except
  , MonadError
  , runExceptT
  , runExcept
  , throwError
  , catchError
  )

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )

import Network.HTTP.Conduit (withManager)

import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Monad (void)
import Data.Foldable (find)
import Data.Text (Text, pack, unpack)

import System.Environment (getArgs)

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

main :: IO ()
main = void . runApp conf $ app
  `catchError` (\e -> liftIO $ print e)

conf :: AppConf
conf = AppConf

app :: App ()
app = do
    ip <- inApp detectIP
    (zone, domain) <- inApp =<< parseArgs <$> liftIO getArgs
    liftIO . putStrLn $ "Updating to: " ++ unpack ip ++ "..."
    let zone' = Domain zone
        domain' = Domain domain
        ip' = ResourceRecord ip
    inApp $ updateDns zone' domain' ip'
    liftIO $ putStrLn "Done"

usage :: String
usage = "Usage: Zone Domain Ip"

-- APP --

newtype App a = App { unApp :: ExceptT AppError (ReaderT AppConf IO) a }
  deriving ( Functor, Applicative, Monad
           , MonadError AppError, MonadIO)

runApp :: AppConf -> App a -> IO (Either AppError a)
runApp c = flip runReaderT c . runExceptT . unApp

data AppConf = AppConf
data AppError
  = AppArgumentError ArgParserError
  | AppGetIPError IPDetectorError
  | AppAwsError APIError
  deriving (Show)

class InApp m where
  inApp :: m a -> App a

instance InApp ArgParser where
  inApp a = either (throwError . AppArgumentError) pure $ runArgParser a

instance InApp IPDetector where
  inApp a = do
    App . ExceptT $ either (throwError . AppGetIPError) pure
      <$> ReaderT (const (pure =<< (liftIO $ runIPDetector a)))

instance InApp APIUpdater where
  inApp a = App . ExceptT $ either (throwError . AppAwsError) pure
    <$> ReaderT (const (pure =<< liftIO $ runAPIUpdater a))

-- ArgParser --

newtype ArgParser a = ArgParser { unArgParser :: Except ArgParserError a }
  deriving ( Functor, Applicative, Monad
           , MonadError ArgParserError)

data ArgParserError = ArgParserError
  deriving (Show)

runArgParser :: ArgParser a -> Either ArgParserError a
runArgParser = runExcept . unArgParser

parseArgs :: [String] -> ArgParser (Text, Text)
parseArgs argv = case argv of
                      [zone, domain] -> return (pack zone, pack domain)
                      _ -> throwError ArgParserError

-- IPDetector --

newtype IPDetector a = IPDetector { unIPDetector :: ExceptT IPDetectorError IO a }
  deriving ( Functor, Applicative, Monad
           , MonadError IPDetectorError, MonadIO)

runIPDetector :: IPDetector a -> IO (Either IPDetectorError a)
runIPDetector = runExceptT . unIPDetector

data IPDetectorError = IPDetectorError
                     | GetIPError DNSError
                     deriving (Show)

detectIP :: IPDetector Text
detectIP = do
  resolverIPs <- dnsLookup "resolver1.opendns.com" defaultResolvConf
  resolverIP <- maybe (throwError IPDetectorError) (return . show) (headMay resolverIPs)
  ips <- dnsLookup "myip.opendns.com"
                   defaultResolvConf { resolvInfo = RCHostName resolverIP }
  maybe (throwError IPDetectorError) (return . pack . show) (headMay ips)

dnsLookup :: (MonadIO m, MonadError IPDetectorError m)
          => BS.ByteString -> ResolvConf -> m [IPv4]
dnsLookup host resolv = do
  resp <- liftIO $ do
    rs' <- makeResolvSeed resolv
    withResolver rs' $ \resolver -> lookupA resolver host
  either (throwError . GetIPError) return resp

-- APIUpdater --

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

findZoneId :: MonadIO m
           => Domain -> HostedZones -> m HostedZone
findZoneId domain zones =
  case find (\zone -> hzName zone == domain) zones of
       Nothing -> fail "Not Found"
       Just zone -> return zone

getHostedZones :: MonadIO m
               => m HostedZones
getHostedZones = liftIO $ do
  cfg <- Aws.baseConfiguration
  resp <- withManager $ \mgr ->
    Aws.aws cfg route53 mgr listHostedZones
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
  cfg <- Aws.baseConfiguration
  withManager $ \mgr ->
    Aws.aws cfg route53 mgr $ ChangeResourceRecordSets zoneId comment changes
