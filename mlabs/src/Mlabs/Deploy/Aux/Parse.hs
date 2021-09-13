{-#LANGUAGE UndecidableInstances#-}
module Mlabs.Deploy.Aux.Parse (getEUTXo) 

where

import System.IO
import Prelude as Hask
import Data.ByteString.Lazy.Internal ( packChars )
import Data.Aeson ( decode, Object )
import Data.Aeson.Lens
import Data.Bifunctor (first,second)
import Control.Lens
import GHC.Generics ( Generic )
import qualified Data.HashMap.Lazy as Hm
import qualified Data.Scientific as Scientific

data Transaction = Transaction 
  { t'id :: Hask.String
  , t'ix :: Hask.Int
  , t'llace :: Hask.Float 
  , t'tokens   :: [Tokens]
  }
  deriving Hask.Show

data Value = Value 
  { v'lovelace :: Hask.Float
  , v'tokens   :: [Tokens]
  }  
  deriving Hask.Show

data Tokens = Tokens
  { t'hash :: Hask.String
  , t'name :: Hask.String
  , t'amm  :: Hask.Float
  }
  deriving Hask.Show

getEUTXo :: FilePath -> IO [Transaction]
getEUTXo fileP = do 
    file <- readFile fileP
    let t         = parse file
        hm        = (getIx . read . show) <$> (maybe mempty Hm.toList $ t) ^.. folded . _1
        lovelace'  =  t ^.. folded . folded . key "value"  . key "lovelace" . _Number . to Scientific.toRealFloat 
        tokens    = (filter (\x -> fst x /= "lovelace") . Hm.toList) <$> (t ^.. folded . folded . key "value" . _Object)  
        tokens'   = fmap (unpack . fst) <$> tokens 
        tokens''  = aux  <$> tokens
        tokens''' = aux' <$> tokens
        tokensf'   = zipWith3 (zipWith3 Tokens) tokens' tokens'' tokens'''
    return $ (Hask.uncurry. Hask.uncurry . Hask.uncurry) Transaction <$> zip (zip hm lovelace') tokensf'
    where 
        getIx :: String -> (String,Int)
        getIx =  second (\x -> (read x) :: Int) . splitAt '#' 
          where 
            splitAt :: Char -> String -> (String,String)
            splitAt = aux (mempty,mempty)
              where
                aux :: (String,String) -> Char -> String -> (String,String)
                aux prev c = \case 
                  (x:xs) -> if c == x then second (const xs) prev else aux (first (x:) prev) c xs
                  _      -> prev 
                
        aux x  = x ^..  folded . folded . _Object . to Hm.toList . folded . to fst . to unpack
        aux' x = x ^..  folded . folded . _Object . to Hm.toList . folded . to snd . _Number . to Scientific.toRealFloat 

        unpack :: Show a =>  a -> String
        unpack = read . show

        parse :: String -> Maybe Object
        parse str = decode . packChars $ str   

testGetEUTXo :: IO [Transaction]
testGetEUTXo = getEUTXo "./deploy/test3.json"