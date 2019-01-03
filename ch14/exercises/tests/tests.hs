module Main where

import qualified ExercisesTest  as ET
import qualified WordNumberTest as WNT

main :: IO ()
main = do
  WNT.main
  ET.main
