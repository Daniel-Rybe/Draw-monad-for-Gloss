module Main where

import           Draw
import           Examples

bg :: Color
bg = makeColorI 150 150 150 255

width :: Float
height :: Float
(width, height) = (640, 480)

window :: Display
window = InWindow "Examples" (round width, round height) (300, 150)

main :: IO ()
main = display window bg combinedExamples

