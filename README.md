


```
                           ,                     _
                          /   _, _   /  _/ _,   / ) _  _,
                         (__ (/ //) () (/ (/   (__ //)_)

                   developer friendly :: type safe :: performant
```


# Rationale

LambdaCms is a set of libraries that contain sub-sites for the
[Yesod application framework](http://www.yesodweb.com).  The LambdaCms
sub-sites can be composed to quickly create a performant website with
content management funcationality.
The `lambdacms-*` packages contain the sub-sites and should be installed
in a "master application" (a standard Yesod application) by adding them as
dependencies to the master application's cabal file and writing some glue code.

The master application is responsible for the database connection, the model
definitions, the authentication strategies, and overrides default behavior
of the `lambdacms-*` packages.

The `lambdacms-*` packages each provide some specific behavior and can in turn
depend on eachother.  The only mandatory package is `lambdacms-core`, this package,
it provides the elementary CMS functionality that all other `lambdacms-*` packages
extend on.


# Disclaimer

This software is in the "alpha" stage: it may break, it may change, it is
certainly not feature complete.

# Installation

In this installation guide we expect the project to named `YourApp`, obviously
you want to replace that with the name of your project (or keep it in case you
are just testing it out and want the convenience of blind copy-pasting the
instructions that follow).


## Haskell & co

Make sure to have **GHC** 7.8.3+, **cabal-install** 1.20+, **happy**, **alex**
and **yesod-bin** 1.4.1+ installed, and their binaries available to your shell's `$PATH`.

To get GHC and Cabal ready we used Yann Esposito's [Safer Haskell Install](http://yannesposito.com/Scratch/en/blog/Safer-Haskell-Install/)
script. This script can be used for Linux and OSX and will install GHC and Cabal.
It also enables the usage of stackage to make sure all libraries which are
installed are compatible. We are currently using the following stackage repository:

    remote-repo: stackage-2014-12-04-ghc76-exc:http://www.stackage.org/snapshot/2014-12-04-ghc76-exc

We recommend using this same stackage repository when working with LambdaCms. To
do so you should change the stackage url currently used by the install script to
this one. Already working with a different stackage or not using stackage at all?
Don't worry! Switching to a different stackage repositories is documented on the
[stackage information page](http://www.stackage.org/stackage/1ee16e7b56ea1e1ac4e15ce7a1cc72018b2117c1)
with 4 easy steps. To start using stackage those same 4 steps can be used.

After installing GHC and Cabal just run the following command to install happy, alex
and yesod-bin:

    cabal install happy alex yesod-bin

To check that you are good to go:

    ghc -V
    cabal -V
    happy -V
    alex -V
    yesod version

## Yesod with Stackage

Scaffold the master application, this interactively takes some configuration values and
initializes a new yesod project inside a folder. This folder's name is based on the project
name you picked, we used `YourApp`. After scaffolding move into the project folder.

    yesod init
    cd YourApp

If you have chosen a database other then Sqlite, then configure it in one of the `.yml` files
the can be found in the `config/` directory.  Also make sure the database can be accessed with
the given configuration -- for `lambdacms` we've chosen postgres w/o fay.

Install the dependencies and run the app in development mode.

    cabal install --enable-tests . --max-backjumps=-1 --reorder-goals
    yesod devel

Now test it by pointing the browser to `localhost:3000`.

If all goes well you can procede to the next step: adding LambdaCms to your app.


## Add LambdaCms

At some point the `lambdacms-core` package will be distributed from Hackage.
Currently this is not the case so we install it from github and register it
locally.

    cd ..
    git clone git@github.com:lambdacms/lambdacms-core.git
    cd lambdacms-core
    cabal install
    cd ../YourApp

---

# Wiring

The next thing to do is to wire everything together such that the base app can use all the goods of Core. To do that, several files need to be modified.

Before we begin, you should know that this how-to also describes what you need to do to setup Role-based authorization. LambdaCms.Core requires a Role-based system to be implemented, but you're free to come up with something else than is described below.

The following steps can be copy/pasted (almost) blindly, however cursive text will elaborate a bit about whats happening.

## Cabal file (name depends on the name of your project)

Add to `build-depends`:

```
, lambdacms-core                >= 0.0.7      && < 0.1
, wai                           >= 3.0.2      && < 3.1
```

Add to `library/exposed-modules`:

```
Roles
```

## config/routes

Add the following routes:

```
/admin/auth    AuthR                 Auth        getAuth
/admin/core    CoreAdminR            CoreAdmin   getLambdaCms
/admin         AdminHomeRedirectR    GET
```

## config/settings.yml

Add this line:
*If no user exists, this email address will be inserted. The resulting account will be activated too.*
```
admin: <your email address>
```

## Settings.hs

Add the following property to `AppSettings`:

```haskell
    , appAdmin                  :: Text
```

Add this line to `instance FromJSON AppSettings`:
Make sure you do this in the same order as properties appear in `settings.yml`.

```haskell
        appAdmin                  <- o .: "admin"
```

## config/models

Remove any content created by the scaffold. For me, this is the `User` and `Email` model. And create a `UserRole` model:

```
UserRole
    userId UserId
    roleName RoleName
    UniqueUserRole userId roleName
    deriving Typeable Show
```

## Models.hs

Add the following imports:

```haskell
import Roles
import LambdaCms.Core
```

## Application.hs

Add the following imports:

```haskell
import LambdaCms.Core
import LambdaCms.Core.Settings (generateUUID)
import qualified Network.Wai.Middleware.MethodOverridePost as MiddlewareMOP
```

Add the following function:

```haskell
getAdminHomeRedirectR :: Handler Html
getAdminHomeRedirectR = do
    redirect $ CoreAdminR AdminHomeR
```

Change the function `makeFoundation` to:
*This will create the `admin` user as provided in `settings.yml`. It will also run all needed migrations.*
```haskell
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings' = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager' <- newManager
    appLogger' <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic' <-
        (if appMutableStatic appSettings' then staticDevel else static)
        (appStaticDir appSettings')

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool' = App { appSettings    = appSettings'
                                        , appStatic      = appStatic'
                                        , appHttpManager = appHttpManager'
                                        , appLogger      = appLogger'
                                        , appConnPool    = appConnPool'
                                        , getLambdaCms   = CoreAdmin
                                        }
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger'

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings')
        (pgPoolSize $ appDatabaseConf appSettings')

    let theFoundation = mkFoundation pool
    runLoggingT
        (runSqlPool (mapM_ runMigration [migrateAll, migrateLambdaCmsCore]) pool)
        (messageLoggerSource theFoundation appLogger')

    let admin = appAdmin appSettings'
    madmin <- runSqlPool (getBy (UniqueEmail admin)) pool
    case madmin of
        Nothing -> do
            timeNow <- getCurrentTime
            uuid <- generateUUID
            flip runSqlPool pool $
                insert_ User { userIdent     = uuid
                             , userPassword  = Nothing
                             , userName      = takeWhile (/= '@') admin
                             , userEmail     = admin
                             , userToken     = Nothing
                             , userCreatedAt = timeNow
                             , userLastLogin = Nothing
                             }
        _ -> return ()

    return theFoundation
```

In the function `makeApplication`:

```haskell
-- replace
    return $ logWare $ defaultMiddlewaresNoLogging appPlain

-- with
    return $ logWare $ MiddlewareMOP.methodOverridePost appPlain
```

## Roles.hs (create this file)

```haskell
module Roles where

import ClassyPrelude.Yesod

data RoleName = Admin
              | SuperUser
              | Blogger
              | MediaManager
              deriving (Eq, Ord, Show, Read, Enum, Bounded)

derivePersistField "RoleName"
```

## Foundation.hs

Add the following imports:

```haskell
import LambdaCms.Core
import Roles
import qualified Data.Set                    as S
import qualified Network.Wai                 as W
```

- Change the implementation of `data App = App { ... }` to:
Note that only addition is `getLambdaCms :: CoreAdmin`. This sets up the Core submodule.

```haskell
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static         -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , getLambdaCms   :: CoreAdmin
    }
```

- Change the implementation of `isAuthorized` (in `instance Yesod App`) to:
*This is a 1-stop to see whether someone is allowed to perform an action. It fetches what a user can do and what is required for what they requested to do. As a bonus, it doesn't just use `isWrite` but rather the actual method such as PUT or DELETE.*

```haskell
isAuthorized theRoute _ = do
    mauthId <- maybeAuthId
    wai <- waiRequest
    y <- getYesod
    murs <- mapM getUserRoles mauthId
    return $ isAuthorizedTo y murs $ actionAllowedFor theRoute (W.requestMethod wai)
```

Change the implementation of `getAuthId` (in `instance YesodAuth App`) to:

> this will likely change soon.

```haskell
    getAuthId creds = do
        timeNow <- lift getCurrentTime
        runDB $ do
            x <- getBy $ UniqueEmail $ credsIdent creds
            case x of
                Just (Entity uid _) -> do
                    _ <- update uid [UserLastLogin =. Just timeNow] -- update last login time during the login process
                    return $ Just uid
                Nothing -> return Nothing
```

In `instance YesodAuth App`:

```haskell
-- replace
    loginDest _ = HomeR
    logoutDest _ = HomeR

-- with
    loginDest _ = CoreAdminR AdminHomeR
    logoutDest _ = AuthR LoginR
```

Add the following instance:
*Here we define that everyone (`Unauthenticated`) can preform `GET` requests for the route `HomeR` (which is likely to be just `/`). The same applies to other common routes such as robots.txt. The last pattern is to catch any other route and simply forbids access. This is where you'd want to add your own routes and permissions.*

```haskell
instance LambdaCmsAdmin App where
    type Roles App = RoleName

    actionAllowedFor (FaviconR) "GET" = Unauthenticated
    actionAllowedFor (RobotsR)  "GET" = Unauthenticated
    actionAllowedFor (HomeR)    "GET" = Unauthenticated
    actionAllowedFor (AuthR _)  _     = Unauthenticated
    actionAllowedFor _          _     = Nobody -- allow no one by default.

    coreR = CoreAdminR
    authR = AuthR
    masterHomeR = HomeR

    getUserRoles userId = do
        v <- runDB $ selectList [UserRoleUserId ==. userId] []
        return . S.fromList $ map (userRoleRoleName . entityVal) v

    setUserRoles userId rs = runDB $ do
        deleteWhere [UserRoleUserId ==. userId]
        mapM_ (insert_ . UserRole userId) $ S.toList rs

    adminMenu =  (defaultCoreAdminMenu CoreAdminR)
    renderLanguages _ = ["en", "nl"]
```

---

## Other Packages
Not every package is included in the stackage. These packages will be listed here.

Clone the repo, cd into it and simply run `cabal install`.

Listing of other packages:

* [friendly-time](https://github.com/pbrisbin/friendly-time)

# Links of interest

Obviously the [Yesod book](http://www.yesodweb.com/book) is a must read,
beyond that docs may sometimes be scarse.
Therefore this collection of links that may shed some light on corners of Yesod
that are of particular interest when hacking on LambdaCms.

http://stackoverflow.com/questions/13055611/what-should-the-type-be-for-a-subsite-widget-that-can-be-used-in-a-master-site

http://www.yesodweb.com/book/wiki-chat-example

https://github.com/yesodweb/yesod/tree/master/yesod-auth (probably the most complex subsite example)

https://groups.google.com/forum/#!searchin/yesodweb/persistent$20subsite/yesodweb/r3hf3xKYAmg/dJDPirX-q2MJ

https://github.com/piyush-kurur/yesod-admin (he stopped working on it before the subsite rewrite)
