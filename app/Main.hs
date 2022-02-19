module Main where

import System.Environment
import Text.Printf
import Lib

-- Horth supports two compilation modes:
-- 1. Compilation via piping
-- 2. In-place compilation of a specified file with a .horth extension
main = do
        args <- getArgs
        let (srcf, ctx) = getsrc args
        src <- srcf
        case com $ tokenize ctx src of
          Left e -> loge e
          Right src' -> case ctx of
                          CompilationCtx "stdin" _ _ -> putStrLn src'
                          CompilationCtx f _ _ -> writeFile (outfile f) src'

outfile :: String -> String
outfile ('.':_) = ".s"
outfile (x:xs) = x:outfile xs

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
