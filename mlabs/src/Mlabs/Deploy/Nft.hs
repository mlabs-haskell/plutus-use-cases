module Mlabs.Deploy.Nft where

import Prelude (String, IO, undefined, print, error)
import PlutusTx.Prelude hiding (error)

import Mlabs.Nft.Contract.StateMachine as SM
import Mlabs.Nft.Logic.Types (Act(..), UserAct(..), Nft(..), NftId(..), toNftId, initNft)
import Mlabs.Nft.Contract.Forge as F
import Mlabs.Emulator.Types (UserId(..))

import qualified Plutus.V1.Ledger.Api as Plutus
import Ledger.Typed.Scripts.Validators as VS



import Mlabs.Deploy.Utils

serializeNft txId txIx ownerPkh content outDir = do
  let
    txOutRef = Plutus.TxOutRef 
                (Plutus.TxId txId) 
                txIx
    userId         = UserId $ Plutus.PubKeyHash ownerPkh
    initNftDatum   = initNft txOutRef userId content (1 % 2) (Just 1000)
    nftId          = nft'id initNftDatum
    typedValidator = SM.scriptInstance nftId
    policy         = F.currencyPolicy (validatorAddress typedValidator) nftId
    redeemer = -- stop sell
      UserAct userId $ SetPriceAct Nothing
    stopSellDatum  = initNftDatum {nft'price = Nothing}

  -- DB.writeFile 
  --   (outDir ++ "/token.name")
  --   (let (NftId (Plutus.TokenName n) _) = nftId in Plutus.fromBuiltin n)

  -- LB.writeFile 
  --   (outDir ++ "/init-datum.json")
  --   (toJson initNftDatum)

  -- LB.writeFile 
  --   (outDir ++ "/stop-sell-datum.json")
  --   (toJson stopSellDatum)

  -- LB.writeFile 
  --   (outDir ++ "/stop-sell-redeemer.json")
  --   (toJson redeemer)

  validatorToPlutus (outDir ++ "/NftScript.plutus") 
    (VS.validatorScript typedValidator)

  policyToPlutus (outDir ++ "/NftPolicy.plutus") policy