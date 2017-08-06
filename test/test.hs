import           Disorder.Core.Main

import qualified Test.Ace.Serial

main :: IO ()
main =
  disorderMain [
      Test.Ace.Serial.tests
    ]
