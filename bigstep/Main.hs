import TRS
import Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file]  -> do
      result <- readTRSFile file
      case result of
        Left e    -> error (show e)
        Right trs -> do
          putStr (showTRS trs)
          putStrLn "--"
          putStrLn (show (nf trs (F "main" [])))
    _ -> error "Usage: nf <file.fp>"
