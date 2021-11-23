module Mlabs.System.Console.Utils (
  logAsciiLogo,
  logAction,
  logBalance,
  logMlabs,
) where

-- Note: should this module be modified to Plutus Prelude version?
-- import PlutusTx.Prelude
import Prelude qualified as Hask

import Control.Monad.IO.Class (MonadIO)
import Plutus.V1.Ledger.Value qualified as Value
import System.Console.ANSI (Color (Black, Cyan, Green, Red))

import Mlabs.System.Console.PrettyLogger (LogColor (Standard, Vibrant))
import Mlabs.System.Console.PrettyLogger qualified as Pretty

logMlabs :: MonadIO m => m ()
logMlabs = logAsciiLogo (Vibrant Red) mlabs

mlabs :: Hask.String
mlabs =
  "                                                                           \n\
  \ ███╗   ███╗    ██╗      █████╗ ██████╗ ███████╗ \n\
  \ ████╗ ████║    ██║     ██╔══██╗██╔══██╗██╔════╝ \n\
  \ ██╔████╔██║    ██║     ███████║██████╔╝███████╗ \n\
  \ ██║╚██╔╝██║    ██║     ██╔══██║██╔══██╗╚════██║ \n\
  \ ██║ ╚═╝ ██║    ███████╗██║  ██║██████╔╝███████║ \n\
  \ ╚═╝     ╚═╝    ╚══════╝╚═╝  ╚═╝╚═════╝ ╚══════╝ "

logAsciiLogo :: MonadIO m => LogColor -> Hask.String -> m ()
logAsciiLogo color logo = do
  Pretty.logNewLine
  Pretty.logPrettyBgColor 40 color (Standard Black) logo
  Pretty.logNewLine

logAction :: MonadIO m => Hask.String -> m ()
logAction str = 
  Pretty.logPrettyColorBold (Vibrant Green) (Pretty.withNewLines str)

logBalance :: MonadIO m => Hask.String -> Value.Value -> m ()
logBalance wallet val = do
  Pretty.logNewLine
  Pretty.logPrettyBgColor 40 (Vibrant Cyan) (Standard Black) 
    (wallet Hask.++ " BALANCE")
  Pretty.logNewLine
  Pretty.logPrettyColor (Vibrant Cyan) (formatValue val)
  Pretty.logNewLine

formatValue :: Value.Value -> Hask.String
formatValue v =
  Hask.unlines Hask.$
    Hask.fmap formatTokenValue Hask.$
      Hask.filter ((Hask./= 0) Hask.. (\(_, _, n) -> n)) Hask.$ 
        Value.flattenValue v
  where
    formatTokenValue (_, name, value) =
      case name of
        "" -> Pretty.padRight ' ' 7 "Ada" Hask.++ " " Hask.++ Hask.show value
        --         (Value.TokenName n) -> Pretty.padRight ' ' 7 $ 
        --           Char8.unpack n ++ " " ++ show value
        (Value.TokenName n) -> 
          Pretty.padRight ' ' 7 Hask.$ Hask.show n Hask.++ " " 
            Hask.++ Hask.show value
