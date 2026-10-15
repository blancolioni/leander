module UseRecords where

import Records

recordsPoint :: RecPoint
recordsPoint = RecPoint { recX = 1, recY = 2 }

recordsY :: RecPoint -> Int
recordsY RecPoint { recY = y } = y

recordsMoved :: Int
recordsMoved = recordsY recordsPoint { recY = 5 }
