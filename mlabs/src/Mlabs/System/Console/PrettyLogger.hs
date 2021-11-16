{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Mlabs.System.Console.PrettyLogger (
  LogColor (..),
  LogStyle (..),
  logPretty,
  logPrettyColor,
  logPrettyBgColor,
  logPrettyColorBold,
  withNewLines,
  logNewLine,
  logDivider,
  padLeft,
  padRight,
) where

-- Note: Originally this module does not use Plutus Prelude at all. When trying
-- to qualify the base Prelude and switching to Plutus Prelude, since the 
-- underlying numeric system is different, padLeft & padRight might need to be
-- taken extra care.

-- import PlutusTx.Prelude
import Prelude qualified as Hask

import Control.Monad.IO.Class (MonadIO (..))
import System.Console.ANSI (
  Color,
  ColorIntensity (Dull, Vivid),
  ConsoleIntensity (BoldIntensity),
  ConsoleLayer (Background, Foreground),
  SGR (Reset, SetColor, SetConsoleIntensity),
  setSGR,
 )

-------------------------------------------------------------------------------

data LogStyle = LogStyle
  { bgColor :: LogColor
  , color :: LogColor
  , isBold :: Hask.Bool
  }

data LogColor
  = Vibrant Color
  | Standard Color
  | DefaultColor

defLogStyle :: LogStyle
defLogStyle =
  LogStyle {bgColor = DefaultColor, color = DefaultColor, isBold = Hask.False}

-------------------------------------------------------------------------------

logPretty :: MonadIO m => Hask.String -> m ()
logPretty = logPrettyStyled defLogStyle

logPrettyStyled :: MonadIO m => LogStyle -> Hask.String -> m ()
logPrettyStyled style string = liftIO Hask.$ do
  setSGR Hask.. Hask.foldMap (Hask.$ style) Hask.$
    [ getColorList Hask.. color
    , getBgColorList Hask.. bgColor
    , getConsoleIntensityList Hask.. isBold
    ]

  Hask.putStr string
  setSGR [Reset]
  where
    getColorList color = case color of
      Vibrant x -> [SetColor Foreground Vivid x]
      Standard x -> [SetColor Foreground Dull x]
      _ -> []
    getBgColorList bgColor = case bgColor of
      Vibrant x -> [SetColor Background Vivid x]
      Standard x -> [SetColor Background Dull x]
      _ -> []
    getConsoleIntensityList isBold =
      [SetConsoleIntensity BoldIntensity | isBold]

-- Convenience functions ------------------------------------------------------

logPrettyColor :: MonadIO m => LogColor -> Hask.String -> m ()
logPrettyColor color = logPrettyStyled defLogStyle {color = color}

logPrettyBgColor :: MonadIO m => Hask.Int -> LogColor -> LogColor 
  -> Hask.String -> m ()
logPrettyBgColor minWidth bgColor color str =
  logPrettyStyled
    defLogStyle {bgColor = bgColor, color = color}
    (padRight ' ' minWidth str)

logPrettyColorBold :: MonadIO m => LogColor -> Hask.String -> m ()
logPrettyColorBold color =
  logPrettyStyled defLogStyle {color = color, isBold = Hask.True}

withNewLines :: Hask.String -> Hask.String
withNewLines string = "\n" Hask.++ string Hask.++ "\n"

logNewLine :: MonadIO m => m ()
logNewLine = logPretty "\n"

logDivider :: MonadIO m => m ()
logDivider =
  logPretty Hask.$
    "-----------------------------------------------------------"
      Hask.++ "\n"

padLeft :: Hask.Char -> Hask.Int -> Hask.String -> Hask.String
padLeft char len txt = Hask.replicate (len Hask.- Hask.length txt) char Hask.<> txt

padRight :: Hask.Char -> Hask.Int -> Hask.String -> Hask.String
padRight char len txt = txt Hask.<> Hask.replicate (len Hask.- Hask.length txt) char
