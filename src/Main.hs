module Main where

import System.IO
import System.Process
import Data.List
import Data.List.Split

substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

branchStr :: String -> String
branchStr all@x = intercalate " " (splitOn "^~^" x)

validBranch :: String -> Bool
validBranch x = not (substring "origin/HEAD" x  || substring x "origin/master")

main = do
    (_, Just hout, _, _) <- createProcess (proc "git" [
        "for-each-ref",
        "--format=%(authorname)^~^%(refname:short)^~^%(committerdate:iso8601)",
        "refs/remotes/origin" ]){ std_out = CreatePipe }
    gitOut <- hGetContents hout
    putStrLn (intercalate "\n" (map branchStr (filter validBranch (splitOn "\n" gitOut))))
