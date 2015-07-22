{-# LANGUAGE OverloadedStrings, FlexibleContexts, GeneralizedNewtypeDeriving #-}

module IPDetector where

import qualified Data.ByteString as BS

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )

import Control.Applicative (Applicative)

import Data.Text (Text, pack)

import Safe (headMay)

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

import Control.Monad.Except
  ( ExceptT
  , MonadError
  , runExceptT
  , throwError
  )

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

