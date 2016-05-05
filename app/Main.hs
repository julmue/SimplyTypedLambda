module Main (main) where

import Data.Char(isDigit)
import System.IO (hPutStrLn, stderr, stdout)

import Language.STLambda.Syntax
import Language.STLambda.Semantics
import qualified Language.STLambda.Parser as Parser
import qualified Language.STLambda.Pretty as Pretty


main :: IO ()
main = do
    input <- getContents
    output (process input)

process :: String -> Output
process s =
    case Parser.expression s of
    Left err -> Output Nothing (Just (show err))
    Right e -> case typeCheck e of
        Left err' -> Output Nothing (Just (show err'))
        Right t -> Output (Just pe) (Just pt)
          where
            pt = Pretty.printType t
            pe = Pretty.printExpr . normalOrder $ e

data Output = Output
    { toStdout :: Maybe String
    , toStderr :: Maybe String
    }

output :: Output -> IO ()
output (Output out err) = do
    op stderr err
    op stdout out
  where
    op h (Just s) = hPutStrLn h s
    op _ Nothing = return ()


normalOrder ::Exp String -> Exp String
normalOrder = mkNormalOrder renderFresh

renderFresh :: (String, Int) -> String
renderFresh (n,i) = n ++ show i
