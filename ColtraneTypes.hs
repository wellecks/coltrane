{- ColtraneTypes.hs

Sean Welleck | Yuanfeng Peng | 2013

Defines the types used by Coltrane.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ColtraneTypes where

import Text.Regex as TR
import qualified Data.Text as DT
import qualified Data.ByteString.Char8 as BS hiding (putStrLn)
import Network.HTTP.Types
import Network.HTTP.Types.Method
import Network.Wai
import qualified Network.Wai.Parse as Parse
import Control.Monad.Error
import qualified Control.Monad.State as MS

-- | a Route is composed of:
--
--    * a method  (uses the StdMethod type from Network.HTTP.Types.Method)
--
--    * a path    (defined below)
--
--    * a handler (defined below)
data Route = Route { 
                     method  :: StdMethod,
                     path    :: Path,
                     handler :: Handler 
}

-- | A ResponseState holds a body, headers, and a status. When used
-- with MonadState, this type makes it easy to add headers and change
-- the response status; WAI does not provide simple mechanisms for
-- modifying the headers and status.
data ResponseState = RS {
                     body    :: ResponseBody,
                     headers :: [Header],
                     status  :: Status
}

rsPlus :: ResponseState -> ResponseState -> ResponseState
rsPlus r1 r2 = RS (body r1 ++ body r2) (headers r1 ++ headers r2) (status r2)

-- | The HandlerState contains the parameters, the request object, and
-- the response state.
data HandlerState = HS {
	resp :: ResponseState,
	pms  :: Params,
	req  :: Request
}

-- | Server options for running the application.
data Server = Warp | CGI
   deriving (Show)

-- | Stores the content-type constants for the response headers.
type ContentType = BS.ByteString
ctHTML = BS.pack "text/html"
ctText = BS.pack "text/plain"
ctJSON = BS.pack "application/json"
ctFile = BS.pack "application/octet-stream"

-- | A ResponseBody is a string.
type ResponseBody = String

-- | A path is either a String Literal or a Regular Expression.
data Path = Literal String | RegExp Regex

type ParamKey = String
type ParamValue = String 
-- | Key value pairs of URL parameters.
type Params  = [(ParamKey, ParamValue)] 

-- | Converts [Parse.Param] to Params.
convertBSParams :: [Parse.Param] -> Params
convertBSParams ps = map unpack ps where
  unpack (a, b) = (BS.unpack a, BS.unpack b)

-- | A type alias to make routes intuitive for the user.
type Handler = HandlerM ()

-- | A monad that holds the application's registered routes.
newtype ColtraneApp a = C { runCA :: MS.StateT [Route] IO a }
	deriving (Monad, MS.MonadState [Route])

-- | Stores the current parameters, Request, and the
-- ResponseState that gets 'built up' in a Handler; the response body, 
-- headers, and status may be altered. The HandlerM also has 
-- error handling capabilities. The Handler is the third component of a Route;
-- a Method and a Path are associated with a Handler.
newtype HandlerM a = HM { runHM :: ErrorT String (MS.StateT HandlerState IO) a }
  deriving (Monad, MS.MonadState HandlerState, MonadError String, MonadIO)
