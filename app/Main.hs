module Main where


import System.Environment
import System.Process
import System.Exit
import Text.Printf
import Lib

-- Horth supports two compilation modes:
-- 1. Compilation via piping
-- 2. In-place compilation of a specified file with a .horth extension
main = do
        args <- getArgs
        let (srcf, ctx) = getsrc args
        src <- srcf
        let CompilationCtx f _ _ = ctx
        case com $ tokenize ctx src of
          Left e -> do
                  loge e
                  exitWith (ExitFailure 1)
          Right src' -> do
                  let src'' = "\t.text\n\
                               \.globl\t_start\n\
                               \_start:\n" ++ src' ++ "\n"
                  writeFile (extfile f ".s") src''
                  readProcess "as" ["-g", extfile f ".s", "-o", extfile f ".o"] []
                  readProcess "ld" [extfile f ".o", "-o", extfile f ""] []
                  clean <- case f of
                            "stdin" -> do
                                    putStrLn <$> readFile f
                                    return [extfile f ".s", extfile f ".o", extfile f ""]
                            _ ->
                                    pure [extfile f ".o"]
                  readProcess "rm" clean []

extfile :: String -> String -> String
extfile ('.':_) ext = ext
extfile (x:xs) ext = x:(extfile xs ext)

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
