module Main
    ( main
    ) where

import Semver
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let parsedArgs = parseArguments args
      errormsg = "Invalid input.\nExpected: stack exec semver [v] MOD1 MOD2\n or: stack exec semver test"
  case parsedArgs of
    Just (b,s1,s2) -> diffModule b s1 s2
    Nothing -> putStrLn errormsg

parseArguments :: [String] -> Maybe (Bool,String,String)
parseArguments args =
  case length args of
    1 -> if head args == "test"
          then Just (True, "./test/Test1.hs", "./test/Test2.hs")
          else Nothing
    2 ->  Just (False,head args, head $ tail args)
    3 -> if head args == "v"
          then Just (True, head $ tail args, head $ tail $ tail args)
          else Nothing
    _ -> Nothing
