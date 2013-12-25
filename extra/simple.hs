import Coltrane
import ColtraneTypes

app :: ColtraneApp ()
app = do
  get (Literal "/") $ do
    html "Hello World!"

  get (Literal "/:song") $ do
    song <- param ":song"
    html $ song ++ " by John Coltrane."

main = coltrane Warp 8000 app