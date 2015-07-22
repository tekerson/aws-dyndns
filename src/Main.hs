{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Trans.Reader
  ( ReaderT (ReaderT)
  , runReaderT
  )

import Control.Monad.Except
  ( ExceptT (ExceptT)
  , MonadError
  , runExceptT
  , throwError
  , catchError
  )

import Control.Monad.IO.Class
  ( MonadIO
  , liftIO
  )

import Control.Applicative (Applicative, pure, (<$>))
import Control.Monad (void)
import Data.Text (unpack)

import System.Environment (getArgs)

import Aws.Route53
  ( Domain (Domain)
  , ResourceRecord (ResourceRecord)
  )

import ArgParser
import IPDetector
import APIUpdater

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
