{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | see: https://tilings.math.uni-bielefeld.de/substitution/cesis-substitution/
module Tile.Cesis where

-- import Control.Lens
-- import Data.Bifunctor (second)
-- import Data.Coerce
-- import Data.Ext
-- import Data.Geometry (EndPoint (Open), IsTransformable (transformBy), LineSegment (LineSegment), Point (Point2), PointFunctor (pmap), Transformation (Transformation), endPoints, pointFromList, toPoints, toVector, translation, uniformScaling, unsafeCoord, vector, xCoord, yCoord, (|.|))
-- import Data.Geometry.Matrix
-- import Data.Geometry.Point (Point, origin)
-- import Data.Geometry.Polygon (SimplePolygon)
-- import qualified Data.Geometry.Polygon as Polygon
-- import Data.Geometry.Triangle
-- import Data.Geometry.Vector
-- import Data.Geometry.Vector.VectorFamily
-- import Data.Geometry.Vector.VectorFamily as Vector hiding (catMaybes)
-- import Data.Maybe (catMaybes, mapMaybe)
-- import Data.Tiling.Class
-- import GHC.Natural

-- -- | A tag for Polyprototiles
-- data PolyProto
--   = P0 Natural
--   | P1 Natural
--   | P2 Natural
--   deriving stock (Show, Eq, Ord)

-- -- | A rectangle centered at the origin
-- recFrom :: (Ord r, Fractional r) => PolyProto -> r -> r -> SimplePolygon PolyProto r
-- recFrom ext' r r' =
--   transformBy (translation $ Vector2 (negate r / 2) (negate r' / 2)) $
--     Polygon.simpleFromPoints $
--       (:+ ext')
--         <$> catMaybes [pointFromList [x, y] | x <- bound, y <- bound]
--   where
--     bound = [0, r, r']

-- squareFrom :: (Ord r, Fractional r) => PolyProto -> r -> SimplePolygon PolyProto r
-- squareFrom ext' r = recFrom ext' r r

-- params :: Floating a => a -> (a, a, a)
-- params r = p
--   where
--     !p = (x, c, s)
--     !x = r / 7
--     !c = cos x
--     !s = sin x

-- p1 :: (Ord r, Floating r) => r -> Natural -> SimplePolygon PolyProto r
-- p1 r n = poly
--   where
--     poly = squareFrom (P1 n) s
--     (_, _, s) = params r

-- p0 :: (Ord r, Floating r) => r -> Natural -> SimplePolygon PolyProto r
-- p0 r n = poly
--   where
--     poly = squareFrom (P0 n) (2 - c - s)
--     (_, c, s) = params r

-- p2 :: (Floating r, Ord r) => r -> Natural -> SimplePolygon PolyProto r
-- p2 r n = poly
--   where
--     poly = recFrom (P2 n) (c + s) (2 - c - s)
--     (_, c, s) = params r

-- refSegTri :: Triangle d p r -> LineSegment d p r
-- refSegTri (Triangle a b _) = LineSegment (Open a) (Open b)

-- refSegPoly :: SimplePolygon p r -> LineSegment 2 p r
-- refSegPoly sp = LineSegment (Open a) (Open b)
--   where
--     a : b : _ = toPoints sp

-- refPtTri :: Triangle d p r -> Point d r :+ p
-- refPtTri (Triangle a _ _) = a

-- refPtPoly :: Polygon.Polygon t p r -> Point 2 r :+ p
-- refPtPoly sp = a
--   where
--     a : _ = toPoints sp

-- rotBtw :: RealFloat r => (t -> LineSegment 2 p r) -> t -> t -> Transformation 2 r
-- rotBtw f ta tb = rotM2d $ angleBetweenSegments (f ta) (f tb)

-- rotBtwTri :: (RealFloat r) => Triangle 2 p r -> Triangle 2 p r -> Transformation 2 r
-- rotBtwTri = rotBtw refSegTri

-- rotBtwPoly :: (RealFloat r) => SimplePolygon p r -> SimplePolygon p r -> Transformation 2 r
-- rotBtwPoly = rotBtw refSegPoly

-- -- | https://math.stackexchange.com/questions/873366/calculate-angle-between-two-lines
-- angleBetweenSegments :: (RealFloat r) => LineSegment 2 p r -> LineSegment 2 p r -> r
-- angleBetweenSegments sa sb = a1 - a2
--   where
--     a1 = atan2 (ay - ay') (ax - ax')
--     a2 = atan2 (by - by') (bx - bx')
--     [a', a] = sa ^.. endPoints
--     ax = a ^. core . xCoord
--     ax' = a' ^. core . xCoord
--     ay = a ^. core . yCoord
--     ay' = a' ^. core . yCoord
--     [b', b] = sb ^.. endPoints
--     bx = b ^. core . xCoord
--     bx' = b' ^. core . xCoord
--     by = b ^. core . yCoord
--     by' = b' ^. core . yCoord

-- -- | Triangle
-- p3 :: (Floating r) => r -> Natural -> Triangle 2 Natural r
-- p3 r n = tri
--   where
--     tri = Triangle (Point2 0 0 :+ n) (Point2 0 c :+ n) (Point2 s 0 :+ n)
--     (_, c, s) = params r

-- rotM2d :: (Floating r) => r -> Transformation 2 r
-- rotM2d theta = Transformation $ Matrix m
--   where
--     !(Just m) = Vector.vectorFromList [row1, row2]
--     !(Just row1) = Vector.vectorFromList [cos theta, negate $ sin theta]
--     !(Just row2) = Vector.vectorFromList [sin theta, cos theta]

-- rotAndTransTri :: RealFloat r => Triangle 2 p r -> Triangle 2 p r -> Transformation 2 r
-- rotAndTransTri t ref_t = trans |.| rotM
--   where
--     rotM = rotBtwTri t ref_t
--     trans = translation ((refPtTri t ^. core . vector) .-. (refPtTri ref_t ^. core . vector))

-- rotAndTransPoly :: RealFloat r => SimplePolygon p r -> SimplePolygon p r -> Transformation 2 r
-- rotAndTransPoly t ref_t = trans |.| rotM
--   where
--     rotM = rotBtwPoly t ref_t
--     trans = translation ((refPtPoly t ^. core . vector) .-. (refPtPoly ref_t ^. core . vector))

-- p3Sub :: (RealFloat r) => r -> Triangle 2 Natural r -> [Triangle 2 Natural r]
-- p3Sub r t = transformBy rotM <$> [ref_t, ref_tr, ref_trr, ref_tru]
--   where
--     (_, c, s) = params r
--     rotM = rotAndTransTri t ref_t
--     ref_t = p3 r 0
--     ref_tr = transformBy (translation (Vector2 s 0) |.| rotM2d 180) ref_t
--     ref_trr = transformBy (rotM2d 180) ref_tr
--     ref_tru = transformBy (translation (Vector2 0 c)) ref_trr

-- p1Sub :: RealFloat r => r -> SimplePolygon PolyProto r -> [SimplePolygon PolyProto r]
-- p1Sub r p = transformBy rotM <$> [ref_bl, ref_br, ref_ur, ref_ul]
--   where
--     (_, _, s) = params r
--     rotM = rotAndTransPoly p ref_bl
--     ref_bl = p1 r 0
--     ref_br = transformBy (translation (Vector2 0 s)) ref_bl
--     ref_ur = transformBy (translation (Vector2 s 0)) ref_br
--     ref_ul = transformBy (translation (Vector2 s 0)) ref_bl

-- p2Sub :: RealFloat r => r -> SimplePolygon PolyProto r -> [SimplePolygon PolyProto r]
-- p2Sub r p = transformBy rotM <$> [ref_bl, ref_br, ref_ur, ref_ul]
--   where
--     (_, c, s) = params r
--     rotM = rotAndTransPoly p ref_bl
--     ref_bl = p2 r 0
--     ref_br = transformBy (translation (Vector2 0 c)) ref_bl
--     ref_ur = transformBy (translation (Vector2 s 0)) ref_br
--     ref_ul = transformBy (translation (Vector2 s 0)) ref_bl

-- p0Sub :: RealFloat r => r -> SimplePolygon PolyProto r -> [Either (Triangle 2 Natural r) (SimplePolygon PolyProto r)]
-- p0Sub r p = transformBy rotM <$> [ref_bl, ref_br, ref_ur, ref_ul]
--   where
--     (_, c, s) = params r
--     rotM = rotAndTransPoly p ref_bl
--     ref_bl = p2 r 0
--     ref_br = transformBy (translation (Vector2 0 c)) ref_bl
--     ref_ur = transformBy (translation (Vector2 s 0)) ref_br
--     ref_ul = transformBy (translation (Vector2 s 0)) ref_bl
