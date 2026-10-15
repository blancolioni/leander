module test_51_records where

--  Records (#33): declaration, construction with labels, selection,
--  update, record patterns, binding more tightly than application, a
--  type used before its declaration, and deriving Show and Read.

data Point = Point { px, py :: Int } deriving (Eq, Show, Read)

data Shape = Circle { centre :: Point, radius :: Int }
           | Rect { corner :: Point, width, height :: Int }
           | Dot Point
           deriving (Eq, Show, Read)

newtype Wrapped = Wrapped { unwrap :: Int } deriving (Show)

origin :: Point
origin = Point { px = 0, py = 0 }

moved :: Point
moved = origin { px = 3 }

area :: Shape -> Int
area (Circle { radius = r }) = 3 * r * r
area Rect { width = w, height = h } = w * h
area _ = 0

isCircle :: Shape -> Bool
isCircle Circle {} = True
isCircle _ = False

later :: Thing
later = Thing { name = 'x', count = 2 }

data Thing = Thing { name :: Char, count :: Int }

readPoint :: String -> Point
readPoint = read

main :: IO ()
main = do
  print (px moved, py moved)
  print (area (Rect { corner = origin, width = 4, height = 5 }))
  print (area (Circle origin 2))
  print (isCircle (Rect origin 1 1), isCircle (Circle { radius = 1 }))
  print (radius (Circle { centre = moved, radius = 7 } { radius = 9 }))
  print (width (Rect origin 1 2) { width = 5, height = 6 })
  print (name later, count later { count = 3 })
  print (px moved { px = 10 })
  print (unwrap (Wrapped 4))
  print (moved == Point 3 0)
  print moved
  print (Just (Rect { corner = moved, width = 1, height = 2 }))
  print (Wrapped { unwrap = 5 })
  print (readPoint "Point {px = 7, py = 8}" == Point 7 8)
  print (read (show (Circle moved 4)) == Circle moved 4)
  print (read " ( Dot Point { px=1 , py=2 } ) " == Dot (Point 1 2))
