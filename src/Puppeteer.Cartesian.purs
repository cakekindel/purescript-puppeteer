module Puppeteer.Cartesian where

import Foreign (Foreign)

foreign import _coord :: Foreign -> Coord
foreign import _box :: Foreign -> Box
foreign import _boxModel :: Foreign -> BoxModel
foreign import _boundingBox :: Foreign -> BoundingBox

type Coord = { x :: Number, y :: Number }

type Box = { tl :: Coord, tr :: Coord, br :: Coord, bl :: Coord }

type BoxModel =
  { border :: Box
  , content :: Box
  , margin :: Box
  , padding :: Box
  , height :: Number
  , width :: Number
  }

type BoundingBox =
  { height :: Number
  , width :: Number
  , coord :: Coord
  }
