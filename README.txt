Coltrane

A Minimal Web Framework for Haskell

Sean Welleck
Yuanfeng Peng

Coltrane is a minimal web framework for Haskell, inspired by
Ruby's Sinatra framework. Coltrane lets you write simple
web applications in just a few lines of Haskell code. 

== Use Coltrane for concise webapps, ...
import Coltrane
import ColtraneTypes

main = coltrane Warp 8000 $ do
         get (Literal "/hello") $ html "Hello World!"

== ... and all that jazz.
$ main
$ == Coltrane has taken the stage ..
$ >> playing on port 8000

Coltrane was built as a final project for CIS552: Advanced Programming,
and is now open-sourced!

== Routes
Routes consist of a path and a handler. 
Paths are either string literals, or regular expressions.

Literal Example

Regular Expression Example

== URL variables
You can create variables in your paths, which can be accessed
using the param function.

== GET and POST parameters
You can also access parameters from GET and POST requests using the
param function.

== HTTP Methods
Get / Post / Put
addRoute

== Content Type Helpers
text / html / json / file

HTML File

== Exceptions
Exceptions

==Modifying the Response
setStatus example

addHeader example

setHeader example


=== Files ===
-- ColtraneTypes.hs --
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

-- Coltrane.hs --
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

-- ColtraneTests.hs --
Contains all of the tests, as well as examples of how to use the framework.
In order to run the tests, the server must be running, so do the following:

Install Dependencies:
  $ cabal install warp==2.0.0.1
  $ cabal install wai-extra==2.0.0.1
  $ cabal install wai==2.0.0

GHCI one (running the server)
  $ :load ColtraneTests
  $ main

GHCI two (running the tests)
  $ :load ColtraneTests
  $ runTests
  
Note that the server is running on port 9000, so you can navigate to pages in the browser, e.g.:
http://localhost:9000
http://localhost:9000/hello
http://localhost:9000/param/firstname/lastname (replace firstname and lastname as desired)

=== Extra Files ===
-- website.hs --
A sample web app built using Coltrane and the Blaze HTML library. Hopefully
it's an example of how easy and concise it is to build a web app using the framework!

To run in ghci:
Install Dependencies (* = installed already if you've run the tests)
* $ cabal install warp==2.0.0.1
* $ cabal install wai-extra==2.0.0.1
* $ cabal install wai==2.0.0
  $ cabal install blaze-html==0.6.1.1
  $ :load website.hs. 
  $ main
  Then navigate to http://localhost:8000 in a web browser.
  
-- index.html --
An HTML file that is used for testing the htmlFile function.

=== Libraries Used / Dependencies ===
wai: A Middleware library that provides a common interface between Coltrane and the underlying web servers.
	http://hackage.haskell.org/package/wai
    
    cabal install wai==2.0.0

wai-extra: Provides additional tools for use with wai. We use it to parse
parameters from a Request body. 
	http://hackage.haskell.org/package/wai-extra
    
    cabal install wai-extra==2.0.0.1

warp: a haskell web server used by WAI.

    cabal install warp==2.0.0.1

Network.HTTP: A simple HTTP interface. Also contains some common types used
in web-related libraries, such as StdMethod, and HeaderName. We also use 
this library to generate requests and responses for testing.

blaze-html (** only used by the sample website.hs **): An HTML combinator
library that allows you to quickly write HTML within Haskell code.

   cabal install blaze-html