{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import GHC.Generics
import System.IO (hPrint, stderr)
import Data.Either
import Control.Concurrent.Async.Pool
import Control.Monad.IO.Class
import Data.Proxy
import Data.Aeson
import Servant.API
import Network.HTTP.Client (newManager, defaultManagerSettings, requestHeaders)
import Servant.Client
import qualified Data.Text as T
import Data.Maybe
import Control.Monad

data Package
  = Package
    { packageName :: String
    , downloads :: Int
    }
    deriving stock Show
    deriving stock Generic
    deriving anyclass FromJSON

type API = "packages" :> "top" :> Get '[JSON] [Package]
      :<|> "package" :> Capture "package" String :> Capture ":cabal.cabal" String :> Get '[PlainText] T.Text

getTop :<|> getCabalFile = client (Proxy @API)

main :: IO ()
main = do
  manager' <- newManager defaultManagerSettings
  let clientEnv0 = mkClientEnv manager' (BaseUrl Http "hackage.haskell.org" 80 "")
      clientEnv  = clientEnv0{makeClientRequest = \b r -> do
                      r' <- makeClientRequest clientEnv0 b r
                      return r'{requestHeaders = [("Accept", "application/json")]}}
  survey (`runClientM` clientEnv)

survey :: Show e => (forall a. ClientM a -> IO (Either e a)) -> IO ()
survey run = withTaskGroup 100 \g -> do
  Right packages <- run getTop
  pkgs' <- flip (mapConcurrently g) packages \pkg -> run do
    buildTypes <- filter ("build-type" `T.isPrefixOf`) . map T.toLower . T.lines <$>
                  getCabalFile pkg.packageName (pkg.packageName ++ ".cabal")
    case buildTypes of
      [] -> return Nothing
      [buildType] -> return
        if isCustom buildType
           then Just pkg.packageName
           else Nothing
      what -> do
        liftIO . putStrLn $ unwords ["more than one build type:", show what]
        return
          if any isCustom what
             then Just pkg.packageName
             else Nothing
  let
      custom_pkgs = catMaybes rpkgs
      (errors, rpkgs) = partitionEithers pkgs'
  unless (null errors) do
    hPrint stderr errors

  putStrLn $ "Total packages:                " ++ show (length packages)
  putStrLn $ "Packages fetched successfully: " ++ show (length rpkgs)
  putStrLn $ "Packages build-type: Custom:   " ++ show (length custom_pkgs)

  print custom_pkgs

isCustom = ("custom" `T.isInfixOf`)
