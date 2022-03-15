-- Monadic interface for drawing pictures with Gloss

{-# LANGUAGE DerivingVia #-}

module Draw where

import           Control.Monad.Trans.State
import           Data.Functor.Identity
import           Data.List
import           Data.Maybe
import           Graphics.Gloss

type PicID = Int

data AppMode
    = ApplyToLast
    | ApplyToGroup [PicID]

data DrawState = DrawState
    { picStack :: [(Picture, PicID)]
    , appMode  :: AppMode
    }

newtype Draw a = Draw { runDraw :: DrawState -> (a, DrawState) }
    deriving (Functor, Applicative, Monad) via State DrawState

render :: Draw a -> Picture
render da = pictures . reverse . map fst . picStack . snd $ runDraw
    da
    (DrawState [] ApplyToLast)

renderM :: Draw a -> Draw PicID
renderM = draw . render

draw :: Picture -> Draw PicID
draw pic = Draw $ \s ->
    let pics = picStack s
        id   = if null pics then 0 else 1 + snd (head pics)
    in  (id, DrawState ((pic, id) : pics) ApplyToLast)

group :: [PicID] -> Draw ()
group ids = Draw $ \s -> ((), s { appMode = ApplyToGroup $ sortDec ids })
    where sortDec = sortOn (0 -)

app :: (Picture -> Picture) -> Draw ()
app f = Draw $ \s ->
    let pics  = picStack s
        mode  = appMode s
        pics' = case mode of
            ApplyToLast      -> applyToLast f pics
            ApplyToGroup ids -> applyToGroup ids f pics
    in  ((), s { picStack = pics' })
  where
    applyToLast _ []                 = []
    applyToLast f ((pic, id) : pics) = (f pic, id) : pics

    applyToGroup [] _ pics = pics
    applyToGroup _  _ []   = []
    applyToGroup (id1 : ids) f ((pic, id2) : pics)
        | id1 == id2 = (f pic, id2) : applyToGroup ids f pics
        | otherwise  = (pic, id2) : applyToGroup (id1 : ids) f pics

lookupId :: PicID -> Draw (Maybe Picture)
lookupId id = Draw
    $ \s -> let pics = picStack s in (lookupPicWithID pics id, s)
  where
    lookupPicWithID [] _ = Nothing
    lookupPicWithID ((pic, id1) : pics) id | id1 == id = Just pic
                                           | otherwise = lookupPicWithID pics id

withId :: PicID -> Draw Picture
withId id =
    let err = error $ "No picture with ID = " ++ show id
    in  fromMaybe err <$> lookupId id

drawId :: PicID -> Draw PicID
drawId id = do
    p <- withId id
    draw p

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

rotateAround :: Float -> Float -> Float -> Picture -> Picture
rotateAround x y a pic = translate x y $ rotate a $ translate (-x) (-y) pic

rotateAroundM :: Float -> Float -> Float -> Draw ()
rotateAroundM x y a = app $ rotateAround x y a
