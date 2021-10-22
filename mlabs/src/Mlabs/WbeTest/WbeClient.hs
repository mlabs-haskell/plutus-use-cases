module Mlabs.WbeTest.WbeClient (
  WbeClientCfg (..),
  defaultWbeClientCfg,
  balance,
  sign,
  submit,
) where

import Control.Monad.IO.Class (MonadIO)

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Mlabs.WbeTest.Types

import Network.HTTP.Req ((/:))
import qualified Network.HTTP.Req as Req

import Prelude

data WbeClientCfg = WbeClientCfg
  { host :: Text
  , port :: Int
  , walletId :: WalletId
  }

defaultWbeClientCfg :: WalletId -> WbeClientCfg
defaultWbeClientCfg = WbeClientCfg "localhost" 8090

balance ::
  MonadIO m =>
  WbeClientCfg ->
  WbeExportTx ->
  m (Either String (WbeTx 'Balanced))
balance cfg = postWallet cfg "transactions-balance"

sign ::
  MonadIO m =>
  WbeClientCfg ->
  WbeTx 'Balanced ->
  m (Either String (WbeTx 'Signed))
sign cfg = postWallet cfg "transactions-sign"

submit ::
  MonadIO m =>
  WbeClientCfg ->
  WbeTx 'Signed ->
  m (Either String (WbeTx 'Signed))
submit WbeClientCfg {..} (WbeTx tx) = eitherDecodeLbs mkReq
  where
    mkReq = Req.req Req.POST url (Req.ReqBodyBs rawTx) Req.lbsResponse options
    url = Req.http host /: "v2" /: "proxy" /: "transactions"
    options = Req.port port <> Req.header "Content-Type" "application/octet-stream"
    rawTx = Text.encodeUtf8 tx

postWallet ::
  ( ToJSON a
  , FromJSON b
  , MonadIO m
  ) =>
  WbeClientCfg ->
  Text ->
  a ->
  m (Either String b)
postWallet WbeClientCfg {..} path reqBody = eitherDecodeLbs mkReq
  where
    mkReq =
      Req.req Req.POST url (Req.ReqBodyJson reqBody) Req.lbsResponse (Req.port port)

    url = Req.http host /: "v2" /: "wallets" /: unWalletId walletId /: path

eitherDecodeLbs ::
  (MonadIO m, FromJSON a) =>
  Req.Req Req.LbsResponse ->
  m (Either String a)
eitherDecodeLbs =
  Req.runReq Req.defaultHttpConfig
    . fmap
      ( eitherDecode
          . Req.responseBody
      )
