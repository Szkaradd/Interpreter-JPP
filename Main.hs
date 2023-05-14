import Interpreter.Program (interpret)
import TypeChecker.Program (typecheck)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Szkarson.Abs (Prog)
import Szkarson.Lex (Token)
import Szkarson.Par (myLexer, pProg)
import Szkarson.Print (Print, printTree)

runProgram :: String -> IO ()
runProgram s =
  let tokens = myLexer s
   in case pProg tokens of
        Left err -> do
          hPutStrLn stderr "Parse Failed:"
          hPutStrLn stderr err
          exitFailure
        Right tree -> do
          checkResult <- typecheck tree
          case checkResult of
            "OK" -> do
              interpretResult <- interpret tree
              case interpretResult of
                Left err -> do
                  hPutStrLn stderr "Interpretation Error:"
                  hPutStrLn stderr err
                  exitFailure
                Right _ -> return ()
            err -> do
              hPutStrLn stderr "Typecheck Failed:"
              hPutStrLn stderr err
              exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> interpretFromFile fileName
    [] -> interpretFromStdin
    _ -> hPutStrLn stderr "Error: too many arguments, expected 0 or 1"

interpretFromFile :: FilePath -> IO ()
interpretFromFile fileName = do
  content <- readFile fileName
  runProgram content

interpretFromStdin :: IO ()
interpretFromStdin = do
  content <- getContents
  runProgram content