Coltrane
=========
#####A Minimal Web Framework for Haskell
---------

Sean Welleck | Yuanfeng Peng

Coltrane is a minimal web framework for Haskell, inspired by
Ruby's Sinatra framework. Coltrane lets you write simple
web applications in just a few lines of Haskell code. 

**Use Coltrane for concise webapps...**
```haskell
import Coltrane
import ColtraneTypes

main = coltrane Warp 8000 $ do
         get (Literal "/hello") $ html "Hello World!"
```
**... and all that jazz.**
```
$ main
$ == Coltrane has taken the stage ..
$ >> playing on port 8000
```

Coltrane was built as a final project for CIS552: Advanced Programming,
and is now open-sourced!

##Routes
Routes consist of a method, a path, and a handler. Paths can be:
- string literals
```haskell
get (Literal "/album") $ do
    text "A Love Supreme"
```

- regular expressions
```haskell
get (RegExp mkRegex "^(/[0-9]+)") $ do
    text "I like numbers."
```

##URL variables
You can create variables in your paths, which can be accessed
using the `param` function:
```haskell
-- e.g. handles a request to /jazz
get (Literal "/:item") $ do
    item <- param ":item"
    html "My favorite thing is: " ++ item

-- e.g. handles a request to /miles/davis
get (Literal "/:first/:last") $ do
    fname <- param ":first"
    lname <- param ":last"
    html "John Coltrane, featuring " ++ fname ++ " " ++ lname
```

##GET and POST parameters
You can also access parameters from GET and POST requests using the
`param` function.
```haskell
-- e.g. handles a GET request to /submit?venue=village
get (Literal "/submit") $ do
    venue <- param "venue"
    html "Live at " ++ venue ++ "."

-- e.g. handles a POST request to /submit with venue=village as a parameter
post (Literal "/submit") $ do
    venue <- param "venue"
    html "Live at " ++ venue ++ "."
```

##HTTP Methods
Coltrane provides helper functions for `get`, `post`, `put`, `delete`.


##Content Type Helpers
Coltrane provides helper functions for `text`, `html`, `json`, `file`.

##HTML Files
Use `htmlFile` to load HTML from a file:
```haskell
get (Literal "/index") $ do
    htmlFile "index.html"
```

##Exceptions
Catch errors with `catchError` and throw exceptions with `throwError`:
```haskell
put (Literal "/trouble") $ do
    (throwError "catch me")
    `catchError`
    (\err -> text $ err ++ " if you can.")

```

##Accessing the Request Data
Access the WAI Request object with `request`:
```haskell
get (Literal "/showpath") $ do 
    req <- request 
    html (show $ pathInfo req)
```
##Modifying the Response
Change the response status code with `setStatus`:

```haskell
get (Literal "/changestatus") $ do
    setStatus status203
    text "Changed on a Moment's Notice"
```
Add an HTTP header with `addHeader`:
```haskell
get (Literal "/addcookie") $ do
    addHeader hCookie "ascension"
    html "Headers up"
```

Alternatively, `setHeader` modifies an existing header's value, or adds a new header if it doesn't exist.

----
##Files
####ColtraneTypes.hs
This file contains the types used in the library. Reading through these
types first will help to understand the structure of the framework.

Notable types include: 
1. ColtraneApp: a monad that stores the application's state. Specifically,
   it stores the user-specified routes that are then used by the router.

2. Route: a method, path, and Handler. 

2. HandlerM: A monad for processing a request and building a response. 
   Stores information that is used and modified within a route handler, and
   is able to throw and catch errors.
   Specifically, contains the request, the parameters, and the response state.
   Intuitively, a Handler consists of 'building' a response state that is then
   turned into a response and sent to the server.

4. HandlerState: describes the state held by the HandlerM.

####Coltrane.hs
This file contains the core functionality of the framework. It contains:
1. Which functions are visible to the user. 

2. The router, which matches a registered route with an incoming request,
   and runs the corresponding handler.

   Functions: router, route

3. The matcher, which is used by the router to perform the actual route
   matching. Matches literal strings, regular expressions, and
   parses url variables.

   Functions: matchesPath

4. Routing functions. Associates a Path and a Handler to create a route,
   which is added to the ColtraneApp state.

   Functions: addroute, addroutes, get, post, put, delete

5. Response modifiers. Changes the response state within a handler. Specifically, changes the headers, the body, and the status.

   Functions: setBody, setStatus, setHeader, addHeader.

6. Request retrieval. Retrieve parameters and the request object.
   
   Functions: param, request.

####ColtraneTests.hs
Contains all of the tests, as well as examples of how to use the framework.
In order to run the tests, the server must be running, so do the following:

Install Dependencies:
```bash
  $ cabal install warp==2.0.0.1
  $ cabal install wai-extra==2.0.0.1
  $ cabal install wai==2.0.0
```
GHCI one (running the server)
```bash
  $ :load ColtraneTests
  $ main
```

GHCI two (running the tests)
```bash
  $ :load ColtraneTests
  $ runTests
```
  
Note that the server is running on port 9000, so you can navigate to pages in the browser, e.g.:
- http://localhost:9000
- http://localhost:9000/hello
- http://localhost:9000/param/firstname/lastname (replace firstname and lastname as desired)

###Extra Files
####website.hs
A sample web app built using Coltrane and the Blaze HTML library. Hopefully
it's an example of how easy and concise it is to build a web app using the framework!

To run in ghci:

**Install Dependencies**
```bash
$ cabal install warp==2.0.0.1
$ cabal install wai-extra==2.0.0.1
$ cabal install wai==2.0.0
$ cabal install blaze-html==0.6.1.1
```
**Run It**
```
$ :load website.hs. 
$ main
```
  Then navigate to http://localhost:8000 in a web browser.

---
##Libraries Used & Dependencies
**wai**: A Middleware library that provides a common interface between Coltrane and the underlying web servers.
    http://hackage.haskell.org/package/wai
    
    cabal install wai==2.0.0

**wai-extra**: Provides additional tools for use with wai. We use it to parse
parameters from a Request body. 
  http://hackage.haskell.org/package/wai-extra
    
    cabal install wai-extra==2.0.0.1

**warp**: a haskell web server used by WAI.

    cabal install warp==2.0.0.1

**Network.HTTP**: A simple HTTP interface. Also contains some common types used
in web-related libraries, such as StdMethod, and HeaderName. We also use 
this library to generate requests and responses for testing.

**blaze-html** (** only used by the sample website.hs **): An HTML combinator
library that allows you to quickly write HTML within Haskell code.
```
cabal install blaze-html
```