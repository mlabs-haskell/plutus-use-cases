module Mlabs.WbeTest.WbeClient (WbeClientCfg(..), balance, sign) where

import Prelude

import Network.HTTP.Req

import Data.Aeson ( decode )
import Data.Text ( Text )

import Mlabs.WbeTest.Types

data WbeClientCfg = WbeClientCfg 
  { url :: Text -- todo not used now
  , walletId :: WalletId
  }

balance :: WbeClientCfg -> WbeExportTx -> IO (Either String (WbeTx Balanced))
balance = _balance
_balance :: WbeClientCfg -> WbeExportTx -> IO (Either String (WbeTx Balanced))  
_balance WbeClientCfg{..} reqBody = runReq defaultHttpConfig $ do
  r <-
    req
      POST
      (http "localhost" /: "v2"/: "wallets" /: walletId /: "transactions-balance")
      (ReqBodyJson reqBody) 
      lbsResponse   
      (port 8090)
  case decode $ responseBody r of
    Nothing -> return $ Left "Failed to read balance reposnse"
    Just wbeTx -> return $ Right wbeTx

sign :: WbeTx Balanced -> IO (Either String (WbeTx Signed))
sign = error "TODO"
