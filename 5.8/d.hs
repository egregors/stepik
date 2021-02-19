import Data.List (intercalate)

data Tree a = Leaf a | Fork (Tree a) a (Tree a)

data TreeDir = Troot | Tleft | Tright

instance Show a => Show (Tree a) where
  show = showTree ("", Troot)
    where
      bi = replicate 8 ' ' -- Base Indent
      bb = '|' : replicate 7 '-' -- Base Branch
      showVal i x = i ++ bb ++ "(" ++ show x ++ ")"

      showTree (i, _) (Leaf x) = showVal i x
      showTree (i, dir) (Fork l x r) =
        let (lSt, rSt) = case dir of
              Troot -> ((' ' : bi, Tleft), (' ' : bi, Tright))
              Tleft -> ((i ++ ' ' : bi, Tleft), (i ++ '|' : bi, Tright))
              Tright -> ((i ++ '|' : bi, Tleft), (i ++ ' ' : bi, Tright))
            lStr = showTree lSt l
            rStr = showTree rSt r
         in intercalate "\n" [lStr, showVal i x, rStr]

fork :: Tree () -> Tree () -> Tree ()
fork = (`Fork` ())

leaf, unit :: Tree ()
leaf = Leaf ()
unit = fork leaf leaf

testTree :: Tree ()
testTree =
  fork
    ( fork
        (fork unit leaf)
        unit
    )
    ( fork
        leaf
        ( fork
            (fork unit unit)
            leaf
        )
    )

numberTree :: Tree () -> Tree Integer
numberTree tree = undefined