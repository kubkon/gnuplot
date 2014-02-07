module Graphics.Gnuplot.Execute where

import Graphics.Gnuplot.Private.OS (gnuplotName, )
import Graphics.Gnuplot.Utility (quote, semiColonConcat, )

import System.Exit (ExitCode, )
import System.Cmd (system, )


simple ::
      [String] {-^ The lines of the gnuplot script to be piped into gnuplot -}
   -> [String] {-^ Options for gnuplot -}
   -> IO ExitCode
simple program options =
   let cmd =
          "sh -c 'echo " ++ quote (semiColonConcat program) ++
                 " | " ++ gnuplotName ++ " " ++ unwords options ++ "'"
   in  do --putStrLn cmd
          system cmd

{-
escape :: String -> String
escape ('\"':xs) = '\\' : '\"' : escape xs
escape (x:xs)    = x : escape xs
escape [] = []
-}
