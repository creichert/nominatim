
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T
import           Options.Applicative

import           Web.Nominatim


data NominatimCli = NominatimCli
  { userAgent :: !String
  , query     :: !String
  , email     :: !String
  , limit     :: !Int
  } deriving Show


nominatimCli :: Parser NominatimCli
nominatimCli = NominatimCli
  <$> strOption
      ( long "user-agent"
      <> metavar "USER-AGENT"
      <> help "User-Agent header value" )
  <*> strOption
      ( long "query"
      <> metavar "ADDRESS"
      <> help "Address query" )
  <*> strOption
      ( long "email"
      <> metavar "EMAIL"
      <> help "Email address to send with request" )
  <*> option auto
      ( long "limit"
      <> metavar "LIMIT"
      <> help "Limit the number of results" )


nominatimRun :: NominatimCli -> IO ()
nominatimRun NominatimCli{..} = do
  r <- nominatim [("User-Agent", BL.pack userAgent)]
                 [ Format Json
                 , Query (T.pack query)
                 , Email (T.pack email)
                 , Limit limit
                 , AddressDetails True
                 ]

  putStrLn $ BL.unpack r
  -- TODO support more stdout output formats
  -- case returnAs of
  --   Json    -> -- print (eitherDecode r :: Either String [Place])
  --   Haskell -> -- print (eitherDecode r :: Either String [Place])
  --   _       -> print r


main :: IO ()
main = execParser opts >>= nominatimRun
  where
    opts = info (helper <*> nominatimCli)
      ( fullDesc
      <> progDesc "\nA simple interface to the OpenStreetMaps Nominatim API. For more details, see http://wiki.openstreetmap.org/wiki/Nominatim"
      <> header "nominatim - a command-line client for nominatim" )
