import           Disorder.Core.Main

import qualified Test.Ace.Data
import qualified Test.Ace.Serial

main :: IO ()
main =
  disorderMain [
      Test.Ace.Data.tests
    , Test.Ace.Serial.tests
    ]
