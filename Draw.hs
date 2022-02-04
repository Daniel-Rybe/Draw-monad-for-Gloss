{-
Monadic interface for composing pictures with the Gloss library.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Draw where

import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromMaybe )
import           Graphics.Gloss                 ( BitmapData
                                                , Color
                                                , Path
                                                , Picture
                                                , Rectangle
                                                , arc
                                                , arcSolid
                                                , bitmap
                                                , bitmapSection
                                                , blank
                                                , circle
                                                , circleSolid
                                                , color
                                                , line
                                                , lineLoop
                                                , pictures
                                                , polygon
                                                , rectangleSolid
                                                , rectangleUpperSolid
                                                , rectangleUpperWire
                                                , rectangleWire
                                                , rotate
                                                , scale
                                                , sectorWire
                                                , text
                                                , thickArc
                                                , thickCircle
                                                , translate
                                                )

type PicID = Int

data AppMode
    = ApplyToPicture -- transformations are applied to the last picture in drawStatePics
    | ApplyToGroup [PicID] -- transformation are applied to pictures with specified ids

data DrawState = DrawState
    { drawStatePics :: [(Picture, PicID)]
    , drawStateMode :: AppMode
    }

newtype Draw a = Draw { runDraw :: DrawState -> (a, DrawState) }
    deriving Functor

instance Applicative Draw where
    pure a = Draw (a, )
    dab <*> da = Draw $ \s ->
        let (ab, s' ) = runDraw dab s
            (a , s'') = runDraw da s'
            b         = ab a
        in  (b, s'')

instance Monad Draw where
    da >>= adb = Draw $ \s ->
        let (a, s')  = runDraw da s
            db       = adb a
            (b, s'') = runDraw db s'
        in  (b, s'')

{-
extract the whole picture from the monadic context
-}
render :: Draw a -> Picture
render da = pictures . reverse . map fst . drawStatePics . snd $ runDraw
    da
    (DrawState [] ApplyToPicture)

{-
Adds a new picture to the stack and sets the mode to ApplyToPicture.
Returns the id of the added picture.
Subsequent transformations will be applied to the last added picture.
-}
draw :: Picture -> Draw PicID
draw pic = Draw $ \s ->
    let pics = drawStatePics s
        id   = if null pics then 0 else 1 + snd (head pics)
    in  (id, DrawState ((pic, id) : pics) ApplyToPicture)

{-
Sets the mode to ApplyToGroup.
Subsequent transformations will be applied to pictures with specified ids.
-}
group :: [PicID] -> Draw ()
group ids = Draw $ \s -> ((), s { drawStateMode = ApplyToGroup $ sortDec ids })
    where sortDec = sortOn (0 -)

{-
Applies a transformation according to the current drawStateMode.
-}
app :: (Picture -> Picture) -> Draw ()
app f = Draw $ \s ->
    let pics  = drawStatePics s
        mode  = drawStateMode s
        pics' = case mode of
            ApplyToPicture   -> applyToHead f pics
            ApplyToGroup ids -> applyToGroup ids f pics
    in  ((), s { drawStatePics = pics' })
  where
    applyToHead _ []                 = []
    applyToHead f ((pic, id) : pics) = (f pic, id) : pics

    applyToGroup [] _ pics = pics
    applyToGroup _  _ []   = []
    applyToGroup (id1 : ids) f ((pic, id2) : pics)
        | id1 == id2 = (f pic, id2) : applyToGroup ids f pics
        | otherwise  = (pic, id2) : applyToGroup (id1 : ids) f pics

{-
Returns the (Maybe Picture) with specified ID,
or Nothing if no such picture exists on the stack.
-}
lookupID :: PicID -> Draw (Maybe Picture)
lookupID id = Draw
    $ \s -> let pics = drawStatePics s in (lookupPicWithID pics id, s)
  where
    lookupPicWithID [] _ = Nothing
    lookupPicWithID ((pic, id1) : pics) id | id1 == id = Just pic
                                           | otherwise = lookupPicWithID pics id

{-
Returns the picture with specified ID.
Throws an error if no such picture is found.
-}
withID :: PicID -> Draw Picture
withID id =
    let err = error $ "No picture with ID = " ++ show id
    in  fromMaybe err <$> lookupID id

-- Monadic counterparts to gloss primitives and transformations
blankM :: Draw PicID
blankM = draw blank

polygonM :: Path -> Draw PicID
polygonM = draw . polygon

lineM :: Path -> Draw PicID
lineM = draw . line

circleM :: Float -> Draw PicID
circleM = draw . circle

thickCircleM :: Float -> Float -> Draw PicID
thickCircleM f1 f2 = draw $ thickCircle f1 f2

arcM :: Float -> Float -> Float -> Draw PicID
arcM f1 f2 f3 = draw $ arc f1 f2 f3

thickArcM :: Float -> Float -> Float -> Float -> Draw PicID
thickArcM f1 f2 f3 f4 = draw $ thickArc f1 f2 f3 f4

textM :: String -> Draw PicID
textM = draw . text

bitmapM :: BitmapData -> Draw PicID
bitmapM = draw . bitmap

bitmapSectionM :: Rectangle -> BitmapData -> Draw PicID
bitmapSectionM r b = draw $ bitmapSection r b

colorM :: Color -> Draw ()
colorM = app . color

translateM :: Float -> Float -> Draw ()
translateM f1 f2 = app $ translate f1 f2

rotateM :: Float -> Draw ()
rotateM = app . rotate

scaleM :: Float -> Float -> Draw ()
scaleM f1 f2 = app $ scale f1 f2

picturesM :: [Picture] -> Draw PicID
picturesM = draw . pictures

-- monadic counterparts to gloss compound shapes
lineLoopM :: Path -> Draw PicID
lineLoopM = draw . lineLoop

circleSolidM :: Float -> Draw PicID
circleSolidM = draw . circleSolid

arcSolidM :: Float -> Float -> Float -> Draw PicID
arcSolidM f1 f2 f3 = draw $ arcSolid f1 f2 f3

sectorWireM :: Float -> Float -> Float -> Draw PicID
sectorWireM f1 f2 f3 = draw $ sectorWire f1 f2 f3

rectangleWireM :: Float -> Float -> Draw PicID
rectangleWireM f1 f2 = draw $ rectangleWire f1 f2

rectangleSolidM :: Float -> Float -> Draw PicID
rectangleSolidM f1 f2 = draw $ rectangleSolid f1 f2

rectangleUpperWireM :: Float -> Float -> Draw PicID
rectangleUpperWireM f1 f2 = draw $ rectangleUpperWire f1 f2

rectangleUpperSolidM :: Float -> Float -> Draw PicID
rectangleUpperSolidM f1 f2 = draw $ rectangleUpperSolid f1 f2

-- Custom primitives and transformations

{-
rotates a picture around a point
-}
rotateAround :: Float -> Float -> Float -> Picture -> Picture
rotateAround x y a pic = translate x y $ rotate a $ translate (-x) (-y) pic

rotateAroundM :: Float -> Float -> Float -> Draw ()
rotateAroundM x y a = app $ rotateAround x y a

