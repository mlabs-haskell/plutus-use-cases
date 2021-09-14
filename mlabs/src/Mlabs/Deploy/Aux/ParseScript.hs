{-#LANGUAGE UndecidableInstances#-}
{-#LANGUAGE TemplateHaskell#-}
module Mlabs.Deploy.Aux.ParseScript 
  ( getEUTXo
  , getMaxLovelace
  , filterToken
  ) 
where

import System.IO ( print, IO, readFile, FilePath )
import Prelude as Hask
import Data.ByteString.Lazy.Internal ( packChars )
import Data.Aeson ( decode, Object, Value (Bool) )
import Data.Aeson.Lens
import Data.Bifunctor (first,second,bimap)
import Control.Lens
import GHC.Generics ( Generic )
import qualified Data.HashMap.Lazy as Hm
import qualified Data.Scientific as Scientific
import Data.List (sortOn)

data TransactionS = TransactionS 
  { _ts'id :: Hask.String
  , _ts'ix :: Hask.Int
  , _ts'addr :: Hask.String
  , _ts'data :: Hask.String
  , _ts'llace :: Hask.Float 
  , _ts'tokens   :: [Tokens]
  }
  deriving Hask.Show

data Tokens = Tokens
  { _ts'hash :: Hask.String
  , _ts'name :: Hask.String
  , _ts'amm  :: Hask.Float
  }
  deriving Hask.Show

makeLenses ''TransactionS
makeLenses ''Tokens

getEUTXo :: FilePath -> IO [TransactionS]
getEUTXo fileP = do 
    file <- readFile fileP
    let t         = parse file
        hm        = (getIx . read . show) <$> (maybe mempty Hm.toList $ t) ^.. folded . _1
        sAddress   = t ^.. folded . folded . key "address" . _Object . to unpack
        sData      = t ^.. folded . folded . key "data" . _Object . to unpack
        lovelace'  =  t ^.. folded . folded . key "value"  . key "lovelace" . _Number . to Scientific.toRealFloat 
        tokens    = (filter (\x -> fst x /= "lovelace") . Hm.toList) <$> (t ^.. folded . folded . key "value" . _Object)  
        tokens'   = fmap (unpack . fst) <$> tokens 
        tokens''  = aux  <$> tokens
        tokens''' = aux' <$> tokens
        tokensf'   = zipWith3 (zipWith3 Tokens) tokens' tokens'' tokens'''
    return $ (Hask.uncurry. Hask.uncurry . Hask.uncurry . Hask.uncurry . Hask.uncurry) TransactionS <$> zip (zip (zip (zip hm sAddress) sData) lovelace') tokensf'
    where 
        getIx :: String -> (String,Int)
        getIx =  bimap reverse (\x -> (read x) :: Int) . splitAt '#' 
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

        unpack :: Show a =>  a -> StringfilterToken
        unpack = read . show

        parse :: String -> Maybe Object
        parse str = decode . packChars $ str   

-- | Returns a list sorted by Lovelace amount
getMaxLovelace :: [TransactionS] -> IO [TransactionS]
getMaxLovelace tx = do
  let a = tx ^.. folded . ts'llace
      b = zip a tx
  return $ snd <$> sortOn fst b  

-- | Filters transactions with a specific token.
filterToken :: Hask.String -> [TransactionS] -> IO [TransactionS]
filterToken tk tx = do
  let tks = tx ^.. folded . ts'tokens 
      b   = zip tks tx
      c   = snd <$> filter aux b
  return c
    where
      aux :: ([Tokens],TransactionS) -> Bool
      aux = foldr (||) False . fmap ((== tk) . view ts'name) . fst

testGetEUTXoS :: IO [TransactionS]
testGetEUTXoS = do
  ux <- getEUTXo "./deploy/gov-script.json"
  tx <- getMaxLovelace ux
  tks <- filterToken "GOV" ux 
  print tx 
  print "sorted" 
  print tks
  print "filtered"
  return ux