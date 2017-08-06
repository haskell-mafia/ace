import           Disorder.Core.Main

import qualified Test.Ace.Data.Web
import qualified Test.Ace.Serial

main :: IO ()
main =
  disorderMain [
      Test.Ace.Data.Web.tests
    , Test.Ace.Serial.tests
    ]
