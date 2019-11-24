import Test.Hspec

import qualified InterpreterSpec

main :: IO ()
main = hspec $ do
  describe "Interpreter" InterpreterSpec.spec
