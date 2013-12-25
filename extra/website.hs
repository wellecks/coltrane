{-# LANGUAGE OverloadedStrings #-}

import Coltrane
import ColtraneTypes
import Text.Blaze.Html.Renderer.String
import Text.Blaze.Html5 as H hiding (html, param)
import Text.Blaze.Html5.Attributes as A

main :: IO ()
main = coltrane Warp 8000 app

app :: ColtraneApp ()
app = do
  get (Literal "/") $ do
    html $ renderHtml home
  
  get (Literal "form") $ do
    html $ renderHtml msgform
  
  get (Literal "thanks") $ do
    msg <- param "msg"
    html $ renderHtml (msgresp msg)

-- uses the Blaze HTML combinator library to define the HTML
home :: Html
home = docTypeHtml $ do
    H.head $ do
        H.title "Coltrane"
    H.body ! A.style "width: 500px; margin: 0px auto 0px auto;" $ do
        h1 $ img ! src "http://i.imgur.com/7YEidGn.jpg" >> "Coltrane"
        h4  "a minimal web framework for Haskell"
        h3 "Sean Welleck | Yuanfeng Peng"
        p $ do
            "Coltrane lets you write simple web apps quickly and easily." >> br
            br >> code "main :: IO ()"
            br >> code "main = coltrane Warp 8000 $ do" >> br
            code20 $ "get (Literal \"/\") $ html \"Hello World!\""
            br >> br
        p $ do
            strong "URL parameters" >> br
            code20 $ "get (Literal \"/:name\") $ do" >> br
            code40 $ "name <- param \":name\""       >> br
            code40 $ "html \"Hello \" ++ name ++ \"!\""
            br >> br
        p $ do
            strong "Regular Expression routes" >> br
            code20 $ "post (RegExp mkRegex \"^(/[0-9]+)\") $ do" >> br
            code40 $ "text \"I like numbers.\""
            br >> br
        p $ do
            strong "Error handling" >> br
            code20 $ "put (Literal \"/trouble\") $ do" >> br
            code40 $ "(throwError \"catch me\")" >> br
            code40 $ "`catchError`" >> br
            code40 $ "(\\err -> text $ err ++ \" if you can.\")"
            br >> br
        p $ do
            strong "And all that jazz" >> br
            code20 $ "$ main" >> br
            code20 $ "$ == Coltrane has taken the stage .." >> br
            code20 $ "$ >> playing on port 8000" >> br
            br >> br >> br
        a ! href "/form" $ "leave a message"
    where code20 = code ! A.style ("margin-left:20px;")
          code40 = code ! A.style ("margin-left:40px;")

msgform :: Html
msgform = docTypeHtml $ do
    H.head $ do
        H.title "Coltrane"
    H.body ! A.style "width: 500px; margin: 0px auto 0px auto;" $ do
        h1 $ img ! src "http://i.imgur.com/7YEidGn.jpg" >> "Coltrane"
        h4  "write a message"
        H.form ! A.method "get" ! A.action "/thanks" $ do
            input ! A.name "msg"
            input ! A.type_ "submit"

msgresp :: String -> Html
msgresp msg = docTypeHtml $ do
    H.head $ do
        H.title "Coltrane"
    H.body ! A.style "width: 500px; margin: 0px auto 0px auto;" $ do
        h1 $ img ! src "http://i.imgur.com/7YEidGn.jpg" >> "Coltrane"
        h4 $ "Thanks for writing " >> toHtml msg
        a ! href "/" $ "go back"