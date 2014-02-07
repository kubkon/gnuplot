module Graphics.Gnuplot.Terminal.PDFCairo (
   T, cons,
   size, font,
   rounded, norounded
   ) where

import qualified Graphics.Gnuplot.Private.Terminal as Terminal
import Graphics.Gnuplot.Utility (quote, formatBool, )

data T =
   Cons {
      filename_ :: FilePath,
      size_ :: Maybe (Int, Int),
      font_ :: Maybe (String, Int),
      rounded_ :: Maybe Bool
   }

cons :: FilePath -> T
cons path =
   Cons {
      filename_ = path,
      size_ = Nothing,
      font_ = Nothing,
      rounded_ = Nothing
   }

size :: Int -> Int -> T -> T
size width height term =
   term{size_ = Just (width, height)}

font :: String -> Int -> T -> T
font fontName fontSize term =
   term{font_ = Just (fontName, fontSize)}

formatFont :: (String, Int) -> String
formatFont (fn,fs) = quote $ fn ++ "," ++ show fs

rounded :: T -> T
rounded term = term{rounded_ = Just True}

norounded :: T -> T
norounded term = term{rounded_ = Just False}

-- private functions

instance Terminal.C T where
   canonical term =
      Terminal.Cons {
         Terminal.options =
            "pdfcairo" :
            (maybe [] (\(w,h) -> "size" : show w : "," : show h : []) $ size_ term) ++
            (maybe [] (\f -> "font" : formatFont f : []) $ font_ term) ++
            (maybe [] (\x -> formatBool "rounded" x : []) $ rounded_ term) ++
            [],
         Terminal.commands =
            ["set output " ++ (quote $ filename_ term)],
         Terminal.interactive = False
      }
