import Data.Char (isSpace)
import Data.Maybe
import Data.Either

-- Code being interpreted immediately, not in a file --
defaultfile = "repl"

-- File name, row, column --
data CompilationCtx = CompilationCtx String Int Int deriving (Show)

-- Reason compilation failed, location of error --
data CompilationError = CompilationError CompilationCtx String deriving (Show)

-- Compile a list of tokens into assembly
com :: [(String, CompilationCtx)] -> Either [CompilationError] String
com [("", _)] = Right ""
com ((x, ctx):xs)
        | isJust $ readMaybe x = Right ("push $" ++ x):com xs
        | otherwise = Left $ CompilationError ctx "invalid symbol `" ++ x ++ "`"
        where prog = com xs

-- Tokenize immediate values
tokenizeimm :: String -> [(String, CompilationCtx)]
tokenizeimm s = tokenize (CompilationCtx defaultfile 0 0) s

tokenize :: CompilationCtx -> String -> [(String, CompilationCtx)]
tokenize ctx s = fst $ foldl advancectx ([], ctx) (split s) 

advancectx :: ([(String, CompilationCtx)], CompilationCtx) -> String -> ([(String, CompilationCtx)], CompilationCtx)
advancectx (acc, ctx) w
        | w == "\n" = (acc, CompilationCtx f (r + 1) 0)
        | w == " "  = (acc, CompilationCtx f r (c + 1))
        | otherwise = (acc ++ [(w, ctx)], CompilationCtx f r (c + length w))
        where CompilationCtx f r c = ctx

-- Splits a string by its whitespaces, including each whitespace as an entry
-- in the resultant list
split :: String -> [String]
split "" = [""]
split (x:xs)
        | isSpace x = "":([x]:(split xs))
        | otherwise = let buf:rem = split xs
                in (x:buf):rem
