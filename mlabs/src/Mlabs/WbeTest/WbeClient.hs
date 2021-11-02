{-# LANGUAGE NamedFieldPuns #-}

module Mlabs.WbeTest.WbeClient (
  balance,
  sign,
  submit,
) where

import Prelude

import Control.Monad.Except (liftEither)
import Control.Monad.Reader (asks)

import Data.Aeson (FromJSON, ToJSON, eitherDecode, object, (.=))
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

import Mlabs.WbeTest.Types

import Network.HTTP.Req ((/:))
import Network.HTTP.Req qualified as Req

balance :: WbeExportTx -> WbeT (WbeTx 'Balanced)
balance = postWallet "transactions-balance"

sign :: WbeTx 'Balanced -> WbeT (WbeTx 'Signed)
sign (WbeTx tx) = do
  WbeClientCfg {passphrase} <- asks wbeClientCfg

  postWallet "transactions-sign" $
    object
      [ "passphrase" .= passphrase
      , "transaction" .= tx
      ]

submit :: WbeTx 'Signed -> WbeT (WbeTx 'Signed)
submit (WbeTx tx) = do
  WbeClientCfg {..} <- asks wbeClientCfg

  let mkReq =
        Req.req
          Req.POST
          url
          (Req.ReqBodyBs rawTx)
          Req.lbsResponse
          options
      url =
        Req.http
          host
          /: "v2"
          /: "proxy"
          /: "transactions"
      options =
        Req.header "Content-Type" "application/octet-stream"
          <> Req.port port
      rawTx = Text.encodeUtf8 tx

  eitherDecodeLbs =<< mkReq

postWallet :: (ToJSON a, FromJSON b) => Text -> a -> WbeT b
postWallet path reqBody = do
  WbeClientCfg {..} <- asks wbeClientCfg

  let mkReq =
        Req.req
          Req.POST
          url
          (Req.ReqBodyJson reqBody)
          Req.lbsResponse
          (Req.port port)
      url =
        Req.http
          host
          /: "v2"
          /: "wallets"
          /: unWalletId walletId
          /: path

  eitherDecodeLbs =<< mkReq

eitherDecodeLbs :: forall a. FromJSON a => Req.LbsResponse -> WbeT a
eitherDecodeLbs =
  liftEither
    . first DecoderError
    . eitherDecode @a
    . Req.responseBody
