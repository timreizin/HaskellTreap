module Treap
( MonoidMul
, Treap
, (><)
, empty
, split
, merge
, insertTreap
, insert --1
, getSum --2
, getValue --3
, getArray --4
, getRange --5
, setRange --6
, setValue --7
, set --8
, addRange --9
, addValue --10
, add --11
, reverseRange --12
, Treap.reverse --13
, Treap.length --14
, pushFront --15
, pushBack --16
, eraseRange --17
, erase --18
, popFront --19
, popBack --20
, fromList
) where


import qualified System.Random as Rand
import Data.Word (Word64)
import System.Random (getStdGen)

class Monoid v => MonoidMul v where
    (><) :: v -> Int -> v

type Lazy value = (Bool, value, value)

data Tree value =
    Empty | Tree {
    value :: value,
    accum :: value,
    lazy :: Lazy value,
    isReversed :: Bool,
    size :: Int,
    priority :: Word64,
    left :: Tree value,
    right :: Tree value}

data Treap value = Treap Rand.StdGen (Tree value)

empty :: Rand.StdGen -> Treap value
empty generator = Treap generator Empty

changeLeft :: Tree value -> Tree value -> Tree value
changeLeft tree newLeft = Tree (value tree) (accum tree) (lazy tree) (isReversed tree) 
                               (size tree) (priority tree) newLeft (right tree)

changeRight :: Tree value -> Tree value -> Tree value
changeRight tree = Tree (value tree) (accum tree) (lazy tree) (isReversed tree) 
                        (size tree) (priority tree) (left tree)

changeValue :: Tree value -> value -> Tree value
changeValue tree newValue = Tree newValue (accum tree) (lazy tree) (isReversed tree) 
                                          (size tree) (priority tree) (left tree) (right tree)

getSize :: Tree value -> Int
getSize Empty = 0
getSize tree = size tree

getAccum :: (Monoid value) => Tree value -> value
getAccum Empty = mempty
getAccum tree = accum tree

applyLazyValue :: Monoid value => value -> Lazy value -> value
applyLazyValue value (isSet, set, add) = t2Value
    where
        tValue = if isSet then set else value
        t2Value = tValue <> add

applyLazyAccum :: MonoidMul value => value -> Lazy value -> Int -> value
applyLazyAccum accum (isSet, set, add) size = t2Accum
    where
        tAccum = if isSet then set >< size else accum
        t2Accum = tAccum <> (add >< size)

applyLazyLazy :: Monoid value => Lazy value -> Lazy value -> Lazy value
applyLazyLazy lazy (isSet, set, add) = tLazy
    where
        tLazy = if isSet 
                then (isSet, set, add) 
                else (let (oIsSet, oSet, oAdd) = lazy in (oIsSet, oSet, oAdd <> add))

applyLazy :: MonoidMul value => Tree value -> Lazy value -> Bool -> Tree value
applyLazy Empty _ _ = Empty
applyLazy tree aLazy aRev = Tree (applyLazyValue (value tree) aLazy) 
                                 (applyLazyAccum (accum tree) aLazy (size tree))
                                 (applyLazyLazy (lazy tree) aLazy) (aRev /= isReversed tree) 
                                 (size tree) (priority tree) (left tree) (right tree)

push :: MonoidMul value => Tree value -> Tree value
push tree = Tree (value tree) (accum tree) (False, mempty, mempty) False (size tree) (priority tree) l r
    where
        tl = applyLazy (left tree) (lazy tree) (isReversed tree)
        tr = applyLazy (right tree) (lazy tree) (isReversed tree)
        (l, r) = if isReversed tree then (tr, tl) else (tl, tr)

pull :: Monoid value => Tree value -> Tree value
pull tree = Tree (value tree) ((getAccum (left tree) <> value tree) <> getAccum (right tree)) 
                 (lazy tree) (isReversed tree) (getSize (left tree) + getSize (right tree) + 1) 
                 (priority tree) (left tree) (right tree)

splitTree :: MonoidMul value => Tree value -> Int -> (Tree value , Tree value)
splitTree Empty _ = (Empty, Empty)
splitTree tree position
    | getSize (left tTree) <= position = let
        (l, r) = splitTree (right tTree) (position - getSize (left tTree) - 1)
        newTree = pull (changeRight tTree l)
        in (newTree, r)
    | otherwise = let
        (l, r) = splitTree (left tTree) position
        newTree = pull (changeLeft tTree r)
        in (l, newTree)
    where
        tTree = push tree

split :: MonoidMul value => Treap value -> Int -> (Treap value, Treap value)
split (Treap generator tree) position = (Treap generatorLeft treeLeft, Treap generatorRight treeRight)
    where
        (treeLeft, treeRight) = splitTree tree position
        (generatorLeft, generatorRight) = Rand.split generator

mergeTree :: MonoidMul value => Tree value -> Tree value -> Tree value
mergeTree treeL Empty = treeL
mergeTree Empty treeR = treeR
mergeTree treeL treeR
    | priority treeL < priority treeR = let tTree = push treeL in
                                        pull (changeRight tTree (mergeTree (right tTree) treeR))
    | otherwise = let tTree = push treeR in pull (changeLeft tTree (mergeTree treeL (left tTree)))

merge :: MonoidMul value => Treap value -> Treap value -> Treap value
merge (Treap generator treeL) (Treap _ treeR) = Treap generator (mergeTree treeL treeR)

insertTreap :: MonoidMul value => Treap value -> Int -> Treap value -> Treap value
insertTreap (Treap generator tree) position (Treap _ treeI) = Treap generator (mergeTree tl r)
    where
        (l, r) = splitTree tree (position - 1)
        tl = mergeTree l treeI

insert :: MonoidMul value => Treap value -> Int -> value -> Treap value
insert (Treap generator tree) position value = Treap nGenerator (mergeTree tl r)
    where
        (l, r) = splitTree tree (position - 1)
        (priority, nGenerator) = Rand.genWord64 generator
        node = Tree value value (False, mempty, mempty) False 1 priority Empty Empty
        tl = mergeTree l node

extractRange tree l r = (lTree, mTree, rTree)
    where
        (tm, rTree) = splitTree tree r
        (lTree, mTree) = splitTree tm (l - 1)

insertRange lTree mTree rTree = nTree
    where 
        nl = mergeTree lTree mTree
        nTree = mergeTree nl rTree

getSum :: MonoidMul value => Treap value -> Int -> Int -> (value, Treap value)
getSum (Treap generator tree) l r = (getAccum mTree, Treap generator nTree)
    where
        (lTree, mTree, rTree) = extractRange tree l r
        nTree = insertRange lTree mTree rTree

getValue :: MonoidMul value => Treap value -> Int -> (value, Treap value)
getValue treap position = getSum treap position position

getArrayTree :: MonoidMul value => Tree value -> ([value], Tree value)
getArrayTree Empty = ([], Empty)
getArrayTree tree = (l ++ [value nTree] ++ r, nTree)
    where
        tTree = push tree
        (l, nl) = getArrayTree (left tTree)
        (r, nr) = getArrayTree (right tTree)
        nTree = changeRight (changeLeft tTree nl) nr

getArray :: MonoidMul value => Treap value -> ([value], Treap value)
getArray (Treap generator tree) = (arr, Treap generator nTree)
    where
        (arr, nTree) = getArrayTree tree

getRange :: MonoidMul value => Treap value -> Int -> Int -> ([value], Treap value)
getRange (Treap generator tree) l r = (arr, Treap generator nTree)
    where
        (lTree, mTree, rTree) = extractRange tree l r
        (arr, nmTree) = getArrayTree mTree
        nTree = insertRange lTree nmTree rTree

getArraySum :: Monoid value => Treap value -> (value, Treap value)
getArraySum treap@(Treap _ tree) = (getAccum tree, treap)

setRange :: MonoidMul value => Treap value -> Int -> Int -> value -> Treap value
setRange (Treap generator tree) l r value = Treap generator nTree
    where
        (lTree, mTree, rTree) = extractRange tree l r
        nmTree = applyLazy mTree (True, value, mempty) False
        nTree = insertRange lTree nmTree rTree

setValue :: MonoidMul value => Treap value -> Int -> value -> Treap value
setValue treap position = setRange treap position position

set :: MonoidMul value => Treap value -> value -> Treap value
set treap@(Treap _ tree) = setRange treap 0 (getSize tree - 1)

addRange :: MonoidMul value => Treap value -> Int -> Int -> value -> Treap value
addRange (Treap generator tree) l r value = Treap generator nTree
    where
        (lTree, mTree, rTree) = extractRange tree l r
        nmTree = applyLazy mTree (False, mempty, value) False
        nTree = insertRange lTree nmTree rTree

addValue :: MonoidMul value => Treap value -> Int -> value -> Treap value
addValue treap position = addRange treap position position

add :: MonoidMul value => Treap value -> value -> Treap value
add treap@(Treap _ tree) = addRange treap 0 (getSize tree - 1)

reverseRange :: MonoidMul value => Treap value -> Int -> Int -> Treap value
reverseRange (Treap generator tree) l r = Treap generator nTree
    where
        (lTree, mTree, rTree) = extractRange tree l r
        nmTree = applyLazy mTree (False, mempty, mempty) True
        nTree = insertRange lTree nmTree rTree

reverse :: MonoidMul value => Treap value -> Treap value
reverse treap@(Treap _ tree) = reverseRange treap 0 (getSize tree - 1)

length :: Treap value -> Int
length (Treap _ tree) = getSize tree

pushFront :: MonoidMul value => Treap value -> value -> Treap value
pushFront treap = insert treap 0 

pushBack :: MonoidMul value => Treap value -> value -> Treap value
pushBack treap@(Treap _ tree) = insert treap (getSize tree) 

eraseRange :: MonoidMul value => Treap value -> Int -> Int -> Treap value
eraseRange (Treap generator tree) l r = Treap generator nTree
    where 
        (lTree, mTree, rTree) = extractRange tree l r
        nTree = insertRange lTree Empty rTree

erase :: MonoidMul value => Treap value -> Int -> Treap value
erase treap position = eraseRange treap position position

popFront :: MonoidMul value => Treap value -> Treap value
popFront treap = erase treap 0

popBack :: MonoidMul value => Treap value -> Treap value
popBack treap@(Treap _ tree) = erase treap (getSize tree - 1)

fromList :: MonoidMul value => Rand.StdGen -> [value] -> Treap value
fromList generator [] = empty generator
fromList generator (f:r) = pushFront (fromList generator r) f

instance (Show value, MonoidMul value) => Show (Treap value ) where
    show = show . fst . getArray

instance (Eq value, MonoidMul value) => Eq (Treap value) where
    (==) treap1 treap2 = fst (getArray treap1) == fst (getArray treap2)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f tree = nTree
        where
            nLazy = let (ff, ss, tt) = lazy tree in (ff, f ss, f tt) 
            nl = fmap f (left tree)
            nr = fmap f (right tree)
            nTree = Tree (f (value tree)) (f (accum tree)) nLazy 
                         (isReversed tree) (size tree) (priority tree) nl nr

instance Functor Treap where
    fmap f (Treap generator tree) = Treap generator (fmap f tree)

--TODO: make into a module, write tests