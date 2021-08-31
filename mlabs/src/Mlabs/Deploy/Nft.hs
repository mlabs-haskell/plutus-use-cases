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
                (Plutus.TxId "8cf79ca29e5d62ad3c62c6f43c38a1eb1db75f0338e3c8ded5c7ca7be910eef3") 
                0
    userId         = UserId $ Plutus.PubKeyHash "4cebc6f2a3d0111ddeb09ac48e2053b83b33b15f29182f9b528c6491"
    initNftDatum   = initNft txOutRef userId "Content" (1 % 2) (Just 1000)
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