module Graphics.Gnuplot.EmbeddedMultiPlot (
  T,
  simpleFromFrameArray,
  simpleFromPartArray,
  title,
  ) where

import qualified Graphics.Gnuplot.Private.Frame as Frame

import qualified Graphics.Gnuplot.Private.Display as Display
import qualified Graphics.Gnuplot.Private.Graph as Graph

import qualified Graphics.Gnuplot.MultiPlot as MultiPlot

import Data.Monoid (mconcat, )
import Data.Foldable (foldMap, )

import Graphics.Gnuplot.Utility (quote, )


data T =
    Cons {
      title_   :: Maybe String,
      size_    :: Double,
      origins_ :: [(Double,Double)],
      parts_   :: [MultiPlot.Part]
    }

simpleFromFrameArray :: Graph.C graph
                     => Double
                     -> [(Double,Double)]
                     -> [Frame.T graph]
                     -> T
simpleFromFrameArray size origins =
    simpleFromPartArray size origins . fmap MultiPlot.partFromFrame

simpleFromPartArray :: Double
                    -> [(Double,Double)]
                    -> [MultiPlot.Part]
                    -> T
simpleFromPartArray size origins parts
  | length origins /= length parts - 1 = error "Num of origins does not match num of insets"
  | otherwise                          = Cons Nothing size origins parts 

title :: String -> T -> T
title str mp =
    mp {title_ = Just str}

instance Display.C T where
    toScript mp =
      mconcat $ (Display.pure $ Display.Body []
                ["set multiplot" ++ foldMap ((" title " ++ ) . quote) (title_ mp)])
              : (MultiPlot.scriptFromPart $ head $ parts_ mp)
              : (Display.pure $ Display.Body []
                ["set size " ++ show (size_ mp)])
              : (zipWith (\o p -> mconcat
                                $ (Display.pure $ Display.Body []
                                  ["set origin " ++ show (fst o) ++ "," ++ show (snd o),
                                   "clear"])
                                : (MultiPlot.scriptFromPart p)
                                : [])
                (origins_ mp)
                $ tail (parts_ mp))
             ++ (Display.pure
                $ Display.Body [] ["unset multiplot"])
              : []
