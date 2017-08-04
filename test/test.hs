import           Disorder.Core.Main
import qualified Test.Ace.Data

main :: IO ()
main =
  disorderMain [
      Test.Ace.Data.tests
    ]
