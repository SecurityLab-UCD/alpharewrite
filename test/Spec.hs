import qualified TestFuncRewrite
import qualified TestTypeVarRewrite

main :: IO ()
main = do
  TestFuncRewrite.testAll
  TestTypeVarRewrite.testAll
