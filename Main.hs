module Main where

import           Draw
import           Examples
import           Graphics.Gloss

bg :: Color
bg = makeColorI 150 150 150 255

width :: Float
height :: Float
(width, height) = (640, 480)

window :: Display
window = InWindow "Examples" (round width, round height) (300, 150)

combinedExamples :: Picture
combinedExamples = render $ do -- 'render' extracts the picture from the Draw monad
    renderM simple -- 'renderM' renders the picture and draws it
    scaleM 0.5 0.5
    translateM (-width / 4) (height / 4)

    renderM simpleRefactored
    scaleM 0.5 0.5
    translateM (width / 4) (height / 4)

    renderM usingIds
    scaleM 0.5 0.5
    translateM (-width / 4) (-height / 4)

    renderM usingGroups
    scaleM 0.5 0.5
    translateM (width / 4) (-height / 4)

    -- some framing
    r1 <- rectangleSolidM 5 height
    r2 <- rectangleSolidM width 5
    group [r1, r2]
    colorM black

main :: IO ()
main = display window bg combinedExamples
