{-#LANGUAGE UndecidableInstances#-}
{-#LANGUAGE TemplateHaskell#-}
module Mlabs.Deploy.Aux.Parse 
  (getEUTXo) 
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

data Transaction = Transaction 
  { _t'id :: Hask.String
  , _t'ix :: Hask.Int
  , _t'llace :: Hask.Float 
  , _t'tokens   :: [Tokens]
  }
  deriving Hask.Show

data Tokens = Tokens
  { _t'hash :: Hask.String
  , _t'name :: Hask.String
  , _t'amm  :: Hask.Float
  }
  deriving Hask.Show

makeLenses ''Transaction
makeLenses ''Tokens

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

        unpack :: Show a =>  a -> String
        unpack = read . show

        parse :: String -> Maybe Object
        parse str = decode . packChars $ str   

-- | Returns a list sorted by Lovelace amount
getMaxLovelace :: [Transaction] -> IO [Transaction]
getMaxLovelace tx = do
  let a = tx ^.. folded . t'llace
      b = zip a tx
  return $ snd <$> sortOn fst b  

-- | Filters transactions with a specific token.
filterToken :: Hask.String -> [Transaction] -> IO [Transaction]
filterToken tk tx = do
  let tks = tx ^.. folded . t'tokens 
      b   = zip tks tx
      c   = snd <$> filter aux b
  return c
    where
      aux :: ([Tokens],Transaction) -> Bool
      aux = foldr (||) False . fmap ((== tk) . view t'name) . fst

testGetEUTXo :: IO [Transaction]
testGetEUTXo = do
  ux <- getEUTXo "./deploy/test3.json"
  tx <- getMaxLovelace ux
  tks <- filterToken "GOV" ux 
  print tx 
  print "sorted" 
  print tks
  print "filtered"
  return ux