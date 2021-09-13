module Mlabs.Deploy.Aux.Parse where

import System.IO
import Prelude as Hask
import Data.ByteString.Lazy.Internal ( packChars )
import Data.Aeson ( FromJSON, ToJSON, decode, Object, encode )
import Data.Aeson.Lens
import Data.Text.Internal
import Control.Lens
import Data.Map (Map)
import GHC.Generics ( Generic )
import qualified Data.HashMap.Lazy as Hm

parse :: String -> Maybe Object
parse str = decode . packChars $ str   

getEUTXo :: IO [String]
getEUTXo = do 
    file <- readFile "deploy/test.json"
    let t = parse file
    putStrLn file
    let contents = t ^.. each
    print contents
    let hm = show <$> (maybe mempty Hm.toList $ t) ^.. folded . _1 
    return hm
