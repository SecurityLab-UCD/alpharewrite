import qualified TestFuncRewrite
import qualified TestTypeVarRewrite
import qualified TestAtomicTypeRewrite

main :: IO ()
main = do
  TestFuncRewrite.testAll
  TestTypeVarRewrite.testAll
  TestAtomicTypeRewrite.testAll
