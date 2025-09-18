import TRS
import TRSParser
import System.Environment

main :: IO ()
main = do
    file : _ <- getArgs
    result <- readTRSFile file
    case result of
        Left e    -> error (show e)
        Right trs -> do
            putStr (showTRS trs)
            -- Output a normal form of F "main" [].
            let mainTerm = F "main" []
            let nfTerm = nf trs mainTerm
            putStrLn("nf: \n" ++ show nfTerm)