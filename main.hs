module Main where

import Data.Char (isSpace)
import Data.Maybe
import Data.Either
import System.Environment
import Text.Read
import Text.Printf

-- Code being interpreted immediately, not in a file --
defaultfile = "stdin"

-- File name, row, column --
data CompilationCtx = CompilationCtx String Int Int deriving (Show)

-- Reason compilation failed, location of error --
data CompilationError = CompilationError CompilationCtx String deriving (Show)

-- Horth supports two compilation modes:
-- 1. Compilation via piping
-- 2. In-place compilation of a specified file with a .horth extension
main :: IO ()
main = do
        args <- getArgs
        let (srcf, ctx) = getsrc args
        src <- srcf
        case com $ tokenize ctx src of
          Left e -> loge e
          Right src' -> case ctx of
                          CompilationCtx "stdin" _ _ -> putStrLn src'
                          CompilationCtx f _ _ -> writeFile f src'

getsrc :: [String] -> (IO String, CompilationCtx)
getsrc [] = getsrc [""]
getsrc [""] = (getContents, CompilationCtx defaultfile 0 0)
getsrc [f] = (readFile f, CompilationCtx f 0 0)

loge :: [CompilationError] -> IO ()
loge [] = do
        return ()
loge (e:es) = do
                printf "%s:%d:%d\t%s\n" f r c msg
                loge es
        where CompilationError (CompilationCtx f r c) msg = e

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
asm ("", _) = Right "mov $0x3c, %rax\n\
                    \mov $0, %rdi\n\
                    \syscall"
asm ("+", _) = Right "pop %rax\n\
                     \pop %rbx\n\
                     \add %rbx, %rax\n\
                     \push %rax\n"
asm (tok, ctx)
        | isJust (readMaybe tok :: Maybe Integer) = Right $ "push $" ++ tok ++ "\n"
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
