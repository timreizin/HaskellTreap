import qualified Data.Monoid as Mon
import qualified System.Random as Rand
import Treap

----Testing----
instance MonoidMul (Mon.Sum Int) where
    (><) a b = Mon.Sum (Mon.getSum a * b)

query1 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query1 treap (_:pos:val:_) = (insert treap pos (Mon.Sum val), "")

query2 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query2 treap (_:l:r:_) = (nTreap, show (Mon.getSum sum) ++ "\n")
    where
        (sum, nTreap) = getSum treap l r

query3 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query3 treap (_:pos:_) = (nTreap, show (Mon.getSum val) ++ "\n")
    where
        (val, nTreap) = getValue treap pos

query4 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query4 treap _ = (nTreap, str ++ "\n")
    where
        (arr, nTreap) = getArray treap
        str = unwords (map (show . Mon.getSum) arr)

query5 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query5 treap (_:l:r:_) = (nTreap, str ++ "\n")
    where
        (arr, nTreap) = getRange treap l r
        str = unwords (map (show . Mon.getSum) arr)

query6 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query6 treap (_:l:r:val:_) = (setRange treap l r (Mon.Sum val), "")

query7 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query7 treap (_:pos:val:_) = (setValue treap pos (Mon.Sum val), "")

query8 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query8 treap (_:val:_) = (set treap (Mon.Sum val), "")

query9 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query9 treap (_:l:r:val:_) = (addRange treap l r (Mon.Sum val), "")

query10 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query10 treap (_:pos:val:_) = (addValue treap pos (Mon.Sum val), "")

query11 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query11 treap (_:val:_) = (add treap (Mon.Sum val), "")

query12 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query12 treap (_:l:r:_) = (reverseRange treap l r, "")

query13 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query13 treap _ = (Treap.reverse treap, "")

query14 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query14 treap _ = (treap, show (Treap.length treap) ++ "\n")

query15 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query15 treap (_:val:_) = (pushFront treap (Mon.Sum val), "")

query16 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query16 treap (_:val:_) = (pushBack treap (Mon.Sum val), "")

query17 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query17 treap (_:l:r:_) = (eraseRange treap l r, "")

query18 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query18 treap (_:pos:_) = (erase treap pos, "")

query19 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query19 treap _ = (popFront treap, "")

query20 :: Treap (Mon.Sum Int) -> [Int] -> (Treap (Mon.Sum Int), String)
query20 treap _ = (popBack treap, "")

processQueries :: Treap (Mon.Sum Int) -> [[Int]] -> String
processQueries treap [] = ""
processQueries treap (f@(t:_):r)
    | t == 1 = let (nTreap, res) = query1 treap f in res ++ processQueries nTreap r
    | t == 2 = let (nTreap, res) = query2 treap f in res ++ processQueries nTreap r
    | t == 3 = let (nTreap, res) = query3 treap f in res ++ processQueries nTreap r
    | t == 4 = let (nTreap, res) = query4 treap f in res ++ processQueries nTreap r
    | t == 5 = let (nTreap, res) = query5 treap f in res ++ processQueries nTreap r
    | t == 6 = let (nTreap, res) = query6 treap f in res ++ processQueries nTreap r
    | t == 7 = let (nTreap, res) = query7 treap f in res ++ processQueries nTreap r
    | t == 8 = let (nTreap, res) = query8 treap f in res ++ processQueries nTreap r
    | t == 9 = let (nTreap, res) = query9 treap f in res ++ processQueries nTreap r
    | t == 10 = let (nTreap, res) = query10 treap f in res ++ processQueries nTreap r
    | t == 11 = let (nTreap, res) = query11 treap f in res ++ processQueries nTreap r
    | t == 12 = let (nTreap, res) = query12 treap f in res ++ processQueries nTreap r
    | t == 13 = let (nTreap, res) = query13 treap f in res ++ processQueries nTreap r
    | t == 14 = let (nTreap, res) = query14 treap f in res ++ processQueries nTreap r
    | t == 15 = let (nTreap, res) = query15 treap f in res ++ processQueries nTreap r
    | t == 16 = let (nTreap, res) = query16 treap f in res ++ processQueries nTreap r
    | t == 17 = let (nTreap, res) = query17 treap f in res ++ processQueries nTreap r
    | t == 18 = let (nTreap, res) = query18 treap f in res ++ processQueries nTreap r
    | t == 19 = let (nTreap, res) = query19 treap f in res ++ processQueries nTreap r
    | t == 20 = let (nTreap, res) = query20 treap f in res ++ processQueries nTreap r

process :: String -> String
process input = processQueries treap queries
    where
        queriesp = map (map (read :: String -> Int) . words) (lines input)
        (start:queries) = queriesp
        treap = fromList (Rand.mkStdGen 3297) (map Mon.Sum start)

main = interact process