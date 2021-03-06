{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ArgParser where

import Data.Text (Text, pack)

import Control.Monad.Except
  ( Except
  , MonadError
  , runExcept
  , throwError
  )

import Control.Applicative (Applicative)

newtype ArgParser a = ArgParser { unArgParser :: Except ArgParserError a }
  deriving ( Functor, Applicative, Monad
           , MonadError ArgParserError)

data ArgParserError = ArgParserError
  deriving (Show)

runArgParser :: ArgParser a -> Either ArgParserError a
runArgParser = runExcept . unArgParser

parseArgs :: [String] -> ArgParser (Text, Text, Maybe Text)
parseArgs argv = case argv of
                      [zone, domain, ip] -> return (pack zone, pack domain, Just (pack ip))
                      [zone, domain] -> return (pack zone, pack domain, Nothing)
                      _ -> throwError ArgParserError
