-- Coltrane, a minimal web framework.
-- Sean Welleck | Yuanfeng Peng | 2013

module Coltrane (
  coltrane,
  get, post, put, delete, addroute, addroutes,
  html, text, json, file, htmlFile,
  setBody, setStatus, setHeader, addHeader,
  param, request,
  throwError, catchError
  ) where

import ColtraneTypes

import qualified Data.ByteString.Lazy.Char8 as LBS hiding (putStrLn, putStr)
import qualified Data.ByteString.Char8 as BS hiding (putStrLn, putStr)
import qualified Data.Text as DT
import Data.Text.Encoding
import Text.Regex
import qualified Control.Monad.State as MS
import Control.Monad.Error

import Network.HTTP.Types
import Network.HTTP.Types.Method
import Network.Wai 
import Network.Wai.Handler.Warp as WA
import Network.Wai.Handler.CGI as CG
import Network.Wai.Parse 

import Data.Either (partitionEithers)
import qualified Data.Text.IO as DTIO (readFile)

-- creates a response with an error message and the
-- status set to 500.
error500 :: String -> HandlerM ()
error500 msg = do text msg
                  setStatus status500
                  return ()

-- the base ResponseState used when execState is called
defaultRS :: ResponseState
defaultRS = RS "" [] status200

-- creates a WAI Response using a ResponseState
renderResponse :: ResponseState -> Response
renderResponse (RS b h s) = responseLBS s h (LBS.pack b)

-- shorthand for unpeeling the HandlerState from the monad
-- if an exception occurs, return an error response with the message
runHandlerM :: HandlerM () -> Params -> Request -> IO HandlerState
runHandlerM rm ps req = MS.execStateT (runErrorT (runHM (rm `catchError` error500))) 
                                     (HS defaultRS ps req)
                

-- run a route's handler on a request
runHandler :: Route -> Params -> Request -> IO ResponseState
runHandler r ps req = do 
  hs <- runHandlerM (handler r) ps req
  return $ resp hs

-- The router is a piece of Middleware, which is just a 
-- function (Application -> Application). Middleware is defined by WAI.
-- The router is 'chained' together with another Application 
-- (here called innerApp); the router tries to route a request 
-- using one of the routes in the input list, and if no route succeeds, 
-- it runs the innerApp, which corresponds to a 404 Not Found.
router :: [Route] -> Middleware
router rs innerApp req = do 
  r <- route rs req 
  case r of
    Nothing     -> innerApp req
    Just rstate -> return $ renderResponse rstate

-- does the actual routing by matching an incoming request's HTTP
-- method and path with one of the routes. if a match is found,
-- the route's handler is run, resulting in a ResponseState.
-- returns Nothing if no matches exist.
route :: [Route] -> Request -> IO (Maybe ResponseState)
route []     _   = return Nothing
route (r:rs) req = 
  if methodMatches r req then
    case path r of 
     Literal l ->
        case matchesPath (trim $ splitPath l) (trim $ pathInfo req) of
         Just ps  -> addPostParams ps r req
         Nothing  -> route rs req
     RegExp  re ->
        case matchRegex re (dropQueryString req) of
          Just strs -> addPostParams (putRegexParams strs) r req
          Nothing -> route rs req
  else
    route rs req 

dropQueryString :: Request -> String
dropQueryString req = 
  let  sr = rawPathInfo req
       (sh:ss) = BS.split '?' $ rawPathInfo req
  in 
       if BS.null sh then BS.unpack sr else BS.unpack sh

addPostParams :: Params -> Route -> Request -> IO (Maybe ResponseState)
addPostParams ps r req = do 
  (ps',_) <- MS.liftIO $ parseRequestBody lbsBackEnd req
  rs <- runHandler r (ps ++ convertBSParams ps') req
  return $ Just rs 

splitPath :: String -> [DT.Text]
splitPath s = DT.split (=='/') (DT.pack s)

-- remove the empties
trim :: [DT.Text] -> [DT.Text]
trim = filter (not . DT.null) 

methodMatches :: Route -> Request -> Bool
methodMatches route req = renderStdMethod (method route)==requestMethod req

putRegexParams :: [String] -> Params
putRegexParams strs = aux 1 strs  where
    aux n []     = []
    aux n (s:ss) = ("r" ++ (show n), s) : (aux (n + 1 ) ss)  


-- matches the path info specified in a route with the path info 
-- in the request
matchesPath :: [DT.Text] -> [DT.Text] -> Maybe Params
matchesPath ((r1:rs1)) ((r2:rs2)) = 
  case DT.unpack r1 of  
    x:_ -> if isWildcard x then 
              combine (Just [(DT.unpack r1, DT.unpack r2)]) matchesRemaining
           else 
              strictlyMatches
    _   -> strictlyMatches 
 where
  isWildcard       = (==':')
  matchesRemaining = matchesPath (rs1) (rs2)
  combine          = liftM2 (++)
  strictlyMatches  = if r1==r2 then matchesRemaining else Nothing

matchesPath r1 r2   = if trim r1 ==trim r2 then Just [] else Nothing 

-- this is 'chained' after the Middleware router in the 
-- main function; thus this runs if the router doesn't find a match
defaultApp :: Application
defaultApp req = return $ renderResponse
                          (RS "404 : Page not Found." [] status404)

-- helper method for adding a GET route
get :: Path -> Handler -> ColtraneApp ()
get p h = addroute $ Route GET p h

---- helper method for adding a POST route
post :: Path -> Handler -> ColtraneApp ()
post p h = addroute $ Route POST p h

---- helper method for adding a PUT route
put :: Path -> Handler -> ColtraneApp ()
put p h = addroute $ Route PUT p h

-- helper method for adding a DELETE route
delete :: Path -> Handler -> ColtraneApp ()
delete p h = addroute $ Route DELETE p h

-- add a route to the app's state
addroute :: Route -> ColtraneApp ()
addroute r = do rs <- MS.get
                MS.put (r:rs)
                return ()

-- add multiple routes to the app's state
addroutes :: [Route] -> ColtraneApp ()
addroutes rs = do 
	st <- MS.get
	MS.put (rs ++ st)
	return ()

-- sets body and content type for HTML
html :: ResponseBody -> HandlerM ()
html = setBody ctHTML

-- sets body and content type for Text
text :: ResponseBody -> HandlerM ()
text = setBody ctText

-- sets body and content type for JSON
json :: ResponseBody -> HandlerM ()
json = setBody ctJSON

-- sets body and content type for File
file :: ResponseBody -> HandlerM ()
file = setBody ctFile

htmlFile :: FilePath -> IO String
htmlFile fp = do h <- (DTIO.readFile fp)
                 return (DT.unpack h)

-- set the current ResponseState's body, and add the
-- corresponding content type header
setBody :: ContentType -> ResponseBody -> HandlerM ()
setBody ct rb = do setHeader hContentType ct
                   (HS (RS _ hs s) pm r) <- MS.get
                   MS.put $ (HS (RS rb hs s) pm r)
                   return ()

-- set the current ResponseState's status
setStatus :: Status -> HandlerM ()
setStatus s = do (HS (RS b h _) pm r) <- MS.get
                 MS.put $ (HS (RS b h s) pm r)
                 return ()

-- lookup a header and set its value to the input string.
-- if the header does not exist, adds a new header.
setHeader :: HeaderName -> BS.ByteString -> HandlerM ()
setHeader hname hval = do 
  (HS (RS b hs s) pm r) <- MS.get
  case lookup hname hs of
    -- if the header exists, replace its value
    Just val -> MS.put $ HS (RS b (replace hname hval hs) s) pm r
    -- otherwise, add a new header
    Nothing  -> addHeader hname hval

replace :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
replace a b ((a', b'):ps) | a == a' = (a, b):ps
                          | otherwise = (a, b) : replace a b ps

-- add a header to the current ResponseState's headers
-- HeaderName defined in Network.HTTP.Types.Header
addHeader :: HeaderName -> BS.ByteString -> HandlerM ()
addHeader hname hval = do (HS (RS b hs s) pm r) <- MS.get
                          MS.put $ HS (RS b ((hname, hval):hs) s) pm r
                          return ()

-- retrieve a field from the querystring in the request
field :: String -> HandlerM String
field key = do HS _ _ req <- MS.get
               case lookup (BS.pack key) (queryString req) of
                Just (Just val) -> return $ BS.unpack val
                _               -> throwError $ msg
  where msg = "Error: Param " ++ key ++ " not found."

-- retrieve a parameter parsed from the URL. if not found,
-- search through the query fields.
param :: String -> HandlerM String
param key = do HS _ ps req <- MS.get
               case lookup key ps of
                Just val -> return val
                Nothing  -> do val' <- field key
                               return val'   
  where msg = "Error: Param " ++ key ++ " not found."

-- retrieve the current request object
request :: HandlerM Request
request = do HS _ _ req <- MS.get
             return req

-- run the framework with the given server on the given port and application
coltrane :: Server -> Port -> ColtraneApp () -> IO ()
coltrane s port capp = do
  putStrLn "== Coltrane has taken the stage .."
  putStr $ ">> playing on port " ++ (show port)
  rs <- MS.execStateT (runCA capp) []
  case s of 
    Warp -> WA.run port (router rs defaultApp)
    CGI  -> CG.run (router rs defaultApp)
