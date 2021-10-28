{-# OPTIONS_GHC -Wno-orphans #-}

module Mlabs.WbeTest.WbeClient (
  WbeClientCfg (..),
  defaultWbeClientCfg,
  balance,
  sign,
  submit,
) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT, except, throwE)

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

import Mlabs.WbeTest.Types

import Network.HTTP.Req ((/:))
import Network.HTTP.Req qualified as Req

import Prelude

data WbeClientCfg = WbeClientCfg
  { host :: Text
  , port :: Int
  , walletId :: WalletId
  }

defaultWbeClientCfg :: WalletId -> WbeClientCfg
defaultWbeClientCfg = WbeClientCfg "localhost" 8090

instance MonadIO m => Req.MonadHttp (ExceptT WbeError m) where
  handleHttpException = throwE . HttpError

balance ::
  MonadIO m =>
  WbeClientCfg ->
  WbeExportTx ->
  ExceptT WbeError m (WbeTx 'Balanced)
balance cfg = postWallet cfg "transactions-balance"

sign ::
  MonadIO m =>
  WbeClientCfg ->
  WbeTx 'Balanced ->
  ExceptT WbeError m (WbeTx 'Signed)
sign cfg = postWallet cfg "transactions-sign"

submit ::
  MonadIO m =>
  WbeClientCfg ->
  WbeTx 'Signed ->
  ExceptT WbeError m (WbeTx 'Signed)
submit WbeClientCfg {..} (WbeTx tx) = eitherDecodeLbs =<< mkReq
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
  ExceptT WbeError m b
postWallet WbeClientCfg {..} path reqBody = eitherDecodeLbs =<< mkReq
  where
    mkReq =
      Req.req Req.POST url (Req.ReqBodyJson reqBody) Req.lbsResponse (Req.port port)

    url = Req.http host /: "v2" /: "wallets" /: unWalletId walletId /: path

eitherDecodeLbs ::
  forall a m. (FromJSON a, MonadIO m) => Req.LbsResponse -> ExceptT WbeError m a
eitherDecodeLbs =
  except
    . first DecoderError
    . eitherDecode @a
    . Req.responseBody
