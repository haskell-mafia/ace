import           Disorder.Core.Main

import qualified Test.Ace.Serial
import qualified Test.Ace.Web

main :: IO ()
main =
  disorderMain [
      Test.Ace.Serial.tests
    , Test.Ace.Web.tests
    ]
