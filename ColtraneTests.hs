{- ColtraneTests.hs

Sean Welleck | Yuanfeng Peng | 2013

Contains tests, and examples of usage.

An application used for testing (testApp) is created 
below, with various routes. Uses HUnit to run the test cases.

A test case consists of making a request made to one of testApp's routes,
and checking the response contents. The requests are made using
the Network.HTTP library. 

In order to run the tests, the testApp must be running.
Run the main function to start the testApp. Then,
run the runTests function to run the tests.

testApp2 defines various routes and handlers that are equivalent
to those defined by testApp, but with different syntax.
-}

{-# LANGUAGE OverloadedStrings #-}

import Coltrane
import ColtraneTypes
import Test.HUnit hiding (Path, State, path)
import Network.HTTP.Types hiding (Header)
import Network.HTTP hiding (GET, POST, PUT, DELETE, HeaderName)
import qualified Control.Monad.State as MS
import qualified Data.ByteString.Char8 as BS hiding (putStrLn)
import Network.Wai.Parse
import Network.Stream
import Text.Regex 
import Network.Wai hiding (Response)

-- appends a path to the base url
make_url :: String -> String
make_url path = "http://localhost:9000/" ++ path

-- sends a GET or POST request to the given URL
-- returns the response and the response body
make_request :: StdMethod -> String -> IO (Result (Response String))
make_request GET  url = simpleHTTP $ getRequest url
make_request POST url = let (u:qs)= BS.split '?' (BS.pack url) in 
  case qs of
    x:_ -> simpleHTTP $ postRequestWithBody (BS.unpack u) "application/x-www-form-urlencoded" (BS.unpack x)
    _ -> simpleHTTP $ postRequest url
make_request _ _ = error "Unsupported request method."

-- make a request and return the response's body and code
response_data :: StdMethod -> String -> IO (String, ResponseCode)
response_data m url = do resp <- make_request m url
                         body <- getResponseBody resp
                         code <- getResponseCode resp
                         return (body, code)

response_all :: StdMethod -> String -> IO (String)
response_all m url = do resp <- make_request m url
                        case resp of
                          Right r -> return $ show r
                          Left  r -> return $ show r
                        

-- test whether a request to the given path returns
-- the expected body and code
test_response :: (StdMethod ,String ,(String, ResponseCode)) -> IO Test
test_response (m ,path, expected) = do 
  actual <- response_data m (make_url path)
  return $ "request body " ++ path ~: expected ~=? actual

-- check whether the response contains the header
test_header :: (StdMethod, String, BS.ByteString) -> IO Test
test_header (m, path, expected) = do
  headers <- response_all m (make_url path)
  putStrLn headers
  return $ "response header " ++ path ~: (BS.isInfixOf expected (BS.pack headers)) ~=? True

-- a test case is a 3-tuple containing:
--    HTTP method
--    path
--    expected output, as a pair:
--      (expected ResponseBody, expected ResponseCode)
testCases = [ (POST, "post?name=John+Coltrane&famous=true", (show testPostParams1, (2,0,0))),
              (POST, "regex/upenn?dpt=cis",                 (show testPostParams2,(2,0,0))),
              (POST, "regex/upenn/seas?dpt=cis",            (show testPostParams3,(2,0,0))),
              (POST, "regex/upenn/1seas?dpt=cis",           ("404 : Page not Found.",(4,0,4))),
              (GET,  "hello",                               ("Hello World!", (2,0,0))),
              (GET,  "fj92i",                               ("404 : Page not Found.", (4,0,4))),
              (GET,  "raise" ,                              ("An error has occurred!", (5,0,0))),
              (GET,  "status",                              ("Status Change", (2,0,3))),
              (GET,  "status2",                             ("Status Change", (2,0,0))),
              (GET,  "param/John/Coltrane",                 ("Hi John Coltrane!", (2,0,0))),
              (GET,  "paramErr/John",                       ("Error: Param :name not found.", (5,0,0))),
              (GET,  "regex/1234",                          ("'{'r1': '/1234'}'",(2,0,0))),
              (POST, "post/employee/company/august/sth",    (show testPostParams4,(2,0,0))),
              (POST, "post/employee/company/august",        ("404 : Page not Found.", (4,0,4)) ),
              (POST, "post",                                ("[]", (2,0,0))),
              (GET,  "catchErr",                            ("Caught the error.", (2,0,0))),
              (GET,  "field?album=Soultrane",               ("Soultrane", (2,0,0))),
              (GET,  "",                                    ("<h1>Giant Steps</h1>", (2,0,0))),
              (GET,  "/",                                   ("<h1>Giant Steps</h1>", (2,0,0)))] 

testHeaderCases = [(GET, "hello", "Content-Type: text/html"),
                   (GET ,"header1", "Cookie: cookie")
                  ]

-- run all of the tests
runTests :: IO Counts
runTests = do ts  <- sequence (map test_response testCases)
              tsh <- sequence (map test_header testHeaderCases)
              runTestTT $ TestList (ts ++ tsh)

testApp :: ColtraneApp ()
testApp = do post (Literal "post")                       postHandler
             post (RegExp wordRegex)                     postHandler 
             get  (Literal "hello")                      helloHandler
             get  (Literal "raise")                      raiseHandler
             get  (Literal "status")                     statusHandler
             get  (Literal "status2")                    status2Handler
             get  (Literal "param/:first/:last")         paramHandler
             get  (Literal "paramErr/:first")            paramErrHandler
             get  (RegExp  numericalRegex )              regexHandler
             post (Literal "post/:arg1/:arg2/:arg3/sth") showParams
             get  (Literal "catchErr")                   catchErrHandler
             get  (Literal "field")                      fieldHandler
             get  (Literal "")                           rootHandler
             get  (Literal "file")                       fileHandler
             get  (Literal "/header1")                   addHeaderTest
             get  (Literal "/htmlFile")                  htmlFileHandler
 
helloHandler :: Handler
helloHandler = html "Hello World!"

raiseHandler :: Handler
raiseHandler = throwError "An error has occurred!"

statusHandler :: Handler
statusHandler = do setStatus status203
                   text "Status Change"

status2Handler :: Handler
status2Handler = do setStatus status203
                    setStatus status404
                    setStatus status200
                    text "Status Change"

rootHandler :: Handler
rootHandler = html "<h1>Giant Steps</h1>"

postHandler :: Handler
postHandler = showParams

htmlFileHandler :: Handler
htmlFileHandler = do h <- MS.liftIO $ htmlFile "extra/index.html"
                     html h

testPostParams4 ::Params
testPostParams4 = [(":arg1","employee"),(":arg2","company"),(":arg3","august")]

testPostParams1 :: Params
testPostParams1 =[("name","John Coltrane"),("famous","true")]

testPostParams2 :: Params
testPostParams2 =[("r1","/upenn"),("dpt","cis")]

testPostParams3 :: Params
testPostParams3 = [("r1","/seas"),("dpt","cis")]

paramHandler :: Handler
paramHandler = do fname <- param ":first"
                  lname <- param ":last"
                  text $ "Hi " ++ fname ++ " " ++ lname ++ "!"

-- the param's name is actually ":first", so an exception is thrown
paramErrHandler :: Handler
paramErrHandler = do name <- param ":name"
                     text $ "Hi " ++ name ++ "!"

-- attempts to get a non-existent parameter, but catches the error
catchErrHandler :: Handler
catchErrHandler = do (do name <- param ":name"
                         text $ "Hi " ++ name ++ "!") 
                     `catchError` 
                     (\err -> text "Caught the error.")

fieldHandler :: Handler
fieldHandler = do album <- param "album"
                  html album

addHeaderTest :: Handler
addHeaderTest = do addHeader hCookie "cookie"
                   html ""

regexHandler :: Handler
regexHandler = do s <- param "r1"
                  json $ "'{'r1': '" ++s ++ "'}'"

fileHandler :: Handler
fileHandler = file "extra/index.html"

--some common regular expressions for testing regex-matching functionality
numericalRegex ::Regex
numericalRegex = mkRegex "^/regex(/[0-9]+)+$" 

wordRegex :: Regex
wordRegex = mkRegex "^/regex(/[a-z]+)+$"   

--some simple handlers facilitating debugging 
showParams :: Handler
showParams =  do 
  hs <- MS.get 
  html $ show (pms hs)

showRawPathInfo :: Handler
showRawPathInfo = do 
  req <- request
  html (BS.unpack $ rawPathInfo req)

showPathInfo :: Handler
showPathInfo = do 
  req <- request 
  html (show $ pathInfo req)

showParsedRequestBody :: Handler
showParsedRequestBody = do
  req <- request 
  rb <- MS.liftIO $ parseRequestBody lbsBackEnd req
  html (show rb)

-- equivalent to testApp, showing an alternative syntax
testApp2 :: ColtraneApp ()
testApp2 = do
  get (Literal "/") $ do
    html "<h1>Giant Steps</h1>"

  get (Literal "hello") $ do
    text "Hello World!"

  get (Literal "raise") $ do
   throwError "An error has occurred!"

  get (Literal "status") $ do
    setStatus status203
    text "Status Change"

  post (Literal "post") $ do
    text "Post Handler"

  get (Literal "file") $ do
    file "extra/index.html"

  get (Literal "param/:first/:last") $ do
    fname <- param ":first"
    lname <- param ":last"
    text $ "Hi " ++ fname ++ " " ++ lname ++ "!"

  get (Literal "paramErr/:first") $ do
    name <- param ":name"
    text $ "Hi " ++ name ++ "!"

  get (Literal "catchErr") $ do
    (do name <- param ":name"
        text $ "Hi " ++ name ++ "!") 
   `catchError` 
   (\err -> text "Caught the error.")

main :: IO ()
main = coltrane Warp 9000 testApp
