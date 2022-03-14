module Examples where

import           Draw
import           Graphics.Gloss

-- example 1
simple :: Draw ()
simple = do
    draw $ circleSolid 150 -- 'draw' draws a picture and returns its id
    app $ color red -- 'app' applied a transformation to the last drawn picture
    app $ translate 20 20

    draw $ rectangleSolid 100 170
    app $ color blue
    app $ translate (-100) (-100)


-- example 2
simpleRefactored :: Draw ()
simpleRefactored = do -- using monadic definitions
    circleSolidM 150
    colorM red
    translateM 20 20

    rectangleSolidM 100 170
    colorM blue
    translateM (-100) (-100)


-- example 3
usingIds :: Draw ()
usingIds = do
    c1 <- circleSolidM 100
    colorM violet
    translateM 200 120

    c2 <- drawId c1 -- 'drawId' draws a copy of the picture with the specified Id.
    translateM (-400) 0

    c3 <- drawId c2
    translateM 0 (-240)

    c4 <- drawId c3
    translateM 400 0

usingGroups :: Draw ()
usingGroups = do
    c1 <- circleSolidM 50
    colorM cyan
    translateM 30 30

    c2 <- circleSolidM 50
    colorM blue
    translateM (-30) (-30)

    let circles = [c1, c2] -- defining group of circles

    r1 <- rectangleSolidM 60 30
    colorM orange
    translateM 45 (-35)

    r2 <- rectangleSolidM 25 55
    colorM azure
    translateM (-35) 25

    let rects = [r1, r2] -- defining group of rectangles

    group circles -- now all transformations are applied to circles
    translateM (-200) 0

    group rects -- now the transformations ate applied to rects
    translateM 200 0


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
