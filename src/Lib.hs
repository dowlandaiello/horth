module Lib
    (
    com,
    asm,
    tokenize,
    tokenizeimm,
    CompilationCtx(CompilationCtx),
    CompilationError(CompilationError),
    defaultfile
    ) where

import Data.Either
import Data.Maybe
import Data.Char (isSpace)
import Text.Read

-- Code being interpreted immediately, not in a file --
defaultfile = "stdin"

-- File name, row, column --
data CompilationCtx = CompilationCtx String Int Int deriving (Show)

-- Reason compilation failed, location of error --
data CompilationError = CompilationError CompilationCtx String deriving (Show)

-- Compile a list of tokens into assembly
com :: [(String, CompilationCtx)] -> Either [CompilationError] String
com [("", ctx)] = asm ("", ctx)
com (x:xs) = case asm x of
        Left [e] -> Left $ e:(fromLeft [] prog)
        Right asm' -> case prog of
                Left e -> Left e
                Right asm'' -> Right $ asm' ++ asm''
        where prog = com xs


asm :: (String, CompilationCtx) -> Either [CompilationError] String
asm ("", _) = Right "\tmov $0x3c, %rax\n\
                    \\tmov $0, %rdi\n\
                    \\tsyscall"
asm ("+", _) = Right "\tpop %rax\n\
                     \\tpop %rbx\n\
                     \\tadd %rbx, %rax\n\
                     \\tpush %rax\n"
asm (tok, ctx)
        | isJust (readMaybe tok :: Maybe Integer) = Right $ "\tpush $" ++ tok ++ "\n"
        | otherwise = Left $ [CompilationError ctx ("invalid symbol `" ++ tok ++ "`")]

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
