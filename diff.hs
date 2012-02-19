module Main where

import System.Environment

-- A line is either present in both files, only in the source, or only in the target:

data Change a = Keep a | Delete a | Insert a deriving (Eq, Show)

showChange (Keep a) = "  " ++ a
showChange (Delete a) = "- " ++ a
showChange (Insert a) = "+ " ++ a

-- To support the snoc operation efficiently, lists are identified with their append functions:

empty = id

toList = flip ($) []

snoc = flip (.) . (:)

-- The diff function works by computing longest common subsequences, and saves intermediate results in a table so that they can be reused. If a value has not been computed yet, then it is represented by Nothing in the table.

-- The following function initializes a table. Along the column header, the edit scripts are just progressive insertions of lines from the target file. Along the row header, the edit scripts are progressive deletions of lines from the source file.

initTableau w h xs ys = 
  let rowHeader = take (1 + w) $ map (\x -> Just (0, x)) $ (scanl (\s x -> snoc (Delete x) s) empty xs) in 
  let row = take w $ repeat Nothing in
  rowHeader : (take h $ map (\y -> (Just (0, y) : row)) $ drop 1 $ (scanl (\s y -> snoc (Insert y) s) empty ys))

-- The update function uses a helper method replace to update a value in the table of intermediate results:

replace idx x xs = let (ys, (z:zs)) = splitAt idx xs in ys ++ [x] ++ zs

update tableau i j value =
  let row = tableau !! j in
  let row' = replace i (Just value) row in
  replace j row' tableau

-- The diff function initializes a tableau, and then recursively computes those pieces of the table needed to calculate an answer:

diff xs ys = 
  let l1 = length xs in
  let l2 = length ys in
  let tableau = initTableau l1 l2 xs ys in
  let tableau' = diff' xs ys l1 l2 tableau in
  let Just (len, lcs) = tableau' !! l2 !! l1 in
  toList lcs

-- This helper method computes the next entry in the table, and returns the modified table:

diff' xs ys i j tableau =
  case tableau !! j !! i of
    Nothing -> 
      let (tableau', value) = step xs ys i j tableau in
      update tableau' i j value
    Just changes -> tableau

-- This method is the workhorse of the diff function, in which the key part of the diff algorithm is implemented. 
-- Namely, we can compute the longest common subsequence function lcs as follows:
--
--   lcs (x:xs) (x':xs') | x == x' = (x:lcs xs xs')
--                       | otherwise = longest (lcs (x:xs) xs') (lcs xs (x':xs'))
--
-- In the ambiguous case where two recrusively calculated sequences are of equal length, we just pick the first.

step xs ys i j tableau = 
  let x = xs !! (i - 1) in
  let y = ys !! (j - 1) in
  if x == y then
    let tableau' = diff' xs ys (i - 1) (j - 1) tableau in
    let Just (len, lcs) = tableau' !! (j - 1) !! (i - 1) in
    (tableau', (1 + len, snoc (Keep x) lcs))
  else
    let tableau' = diff' xs ys (i - 1) j tableau in
    let Just (l1, s1) = tableau' !! j !! (i - 1) in
    let tableau'' = diff' xs ys i (j - 1) tableau' in
    let Just (l2, s2) = tableau'' !! (j - 1) !! i in
    case compare l1 l2 of 
      GT -> (tableau'', (l1, snoc (Delete x) s1))
      _  -> (tableau'', (l2, snoc (Insert y) s2))

-- The main function simply takes two filenames as arguments, applies the diff function, and prints the changes:

main = do
  [old, new] <- getArgs
  xs <- fmap lines $ readFile old 
  ys <- fmap lines $ readFile new 
  let changes = diff xs ys
  putStrLn $ unlines $ map showChange changes
