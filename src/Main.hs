module Main where

import Data.Char
import Data.List

import Tests
import PrecedenceClimbing

loop = do
    str <- getLine
    if null str
    then return ()
    else
        case str of
            "exit" -> return ()
            "test" -> print $ testingPrecedenceClimbing
            _ ->
                let result = compute str
                in do
                    print result
                    loop


-- | The main entry point.
main :: IO ()
main = do
    loop