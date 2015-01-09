


```
                           ,                     _
                          /   _, _   /  _/ _,   / ) _  _,
                         (__ (/ //) () (/ (/   (__ //)_)

                   developer friendly :: type safe :: performant
```


# Rationale

LambdaCms is a bunch of packaged libraries, containing sub-sites for the
[Yesod application framework](http://www.yesodweb.com), which allow rapid
development of robust and highly performant websites with content management
functionality.

The `lambdacms-*` packages each provide some functionality and can depend
on eachother as they depend on other packages.
The only mandatory package is `lambdacms-core` (this package), it provides
functionality that all other `lambdacms-*` packages can rely on.

As mentioned, each `lambdacms-*` package contains a sub-site which is
"mounted" in a standard Yesod application, which we will refer to as
"the base application" or simply "base".
Before a packaged sub-site can be mounted, the package needs to be
included as a dependency to the base app's `.cabal` file.  After that
some glue code needs to be added to the base app, as explained below.

In the base app we have to:
* organize the main menu of the admin backend,
* configure a the database connection,
* specify the authentication strategies, and
* define admin user roles and their permissions.

In the base app we may optionally also:
* override default behavior,
* override UI texts,
* provide a means to send email notifications, and last but not least,
* write the themes so the website can actually be visited (recommended).


# Getting started

Using this guide you will create a CMS website named `YourApp`.
For a real project you want to substitute this name, but for trying
out LambdaCms it is recommended to keep it for the convenience of
copy-pasting the instructions that follow.


### Prerequisites

You need to be reasonably acquinted with Haskell in order to follow
along with this guide. To learn basic Haskell skill we recommend
Brent Yorgey's excellent
[Introduction to Haskell](http://www.seas.upenn.edu/~cis194/spring13)
course.

Besides Haskell you need to be somewhat familliar with:
* the web technologies (HTTP, HTML, CSS, JS, REST),
* SQL (as LambdaCms makes use of a relational database), and
* the Yesod web application framework ([book](http://yesodweb.com/book)).


### The tool chain

Make sure to have **GHC** 7.8.3+, **cabal-install** 1.20+, **happy**, **alex**
and **yesod-bin** 1.4.1+ installed, and their binaries available from your
shell's `$PATH`.

To check that you are good to go, you can use these commands.

    ghc -V
    cabal -V
    happy -V
    alex -V
    yesod version

In case you are **not** good to go, you may want to follow the
[installation guide on the Stackage website](http://www.stackage.org/install)
which provides instructions for all dependencies except `yesod-bin`,
for a variety of platforms.

Once you meet all the requirements except `yesod-bin`, install it.

    cabal install "yesod-bin >= 1.4.1"


### Create the base application

With the following command you create a "scaffolded" Yesod application.
The command is interactive; you need to supply some configuration values.
Pick the database of your choice, and name it `YourApp` (if you want follow this
guide closely).

    yesod init

After scaffolding move into the project folder.

    cd YourApp

If you have chosen a database other then Sqlite, you need to create a
database and a sufficiently priviledged user, and supply the
credentials to the `config/setting.yml` file.


### Specify a Stackage snapshot

To avoid spending too much time on build issues we use and recommend
[LTS Haskell](https://github.com/fpco/lts-haskell#readme).

Currently we develop and test LambdaCms only against the lastest
LTS Haskell release. As minor releases of LTS Haskell should never
contain breaking changes we only provide the major release number,
thereby automatically using the most recent minor release in that
series.

Run the following commands from within `YourApp`'s project folder,
to install the most recent LTS Haskell package set in the `1.x` series.

    wget http://www.stackage.org/lts/1/cabal.config
    cabal update

The install all dependencies and build your application with (this
may take a while the first time you run it).

    cabal install --enable-tests . --max-backjumps=-1 --reorder-goals

In case you experience problems with `cabal install` try adding
`-j1` as a flag (prevents concurrent building).

When you experience problems during builds, while using LTS `1.x`,
we consider this a bug. Please
[raise an issue](https://github.com/lambdacms/lambdacms-core/issues).

The following commands will run your scaffolded Yesdo application
in development mode.

    yesod devel

Now test it by pointing the browser to `localhost:3000`.

If all went well you are ready to add LambdaCms to your app.


### Add LambdaCms

At some point the `lambdacms-core` package will be distributed from Hackage,
until then we install it from Github.

    cd ..
    git clone git@github.com:lambdacms/lambdacms-core.git
    cd lambdacms-core
    cabal install
    cd ../YourApp

In the following sub-sections we explain how to install `lambdacms-core` into
the base application.  Much of what we show here can be accomplished in
many different ways, in this guide we merely provide a way to get you started.


#### Modify the `YourApp.cabal` file

First add `lambdacms-core` to the dependencies list of the `YourApp.cabal`
file (name of the file depends on the name of your project).

Add the follwing to the end of the `build-depends` section:

```
, lambdacms-core                >= 0.0.7      && < 0.1
, wai                           >= 3.0.2      && < 3.1
```

And add the following line to the `library/exposed-modules` section:

```
Roles
```

#### Modify the `config/routes` file

Add the following routes to the `config/routes` file of your application:

```
/admin/auth    AuthR                 Auth        getAuth
/admin/core    CoreAdminR            CoreAdmin   getLambdaCms
/admin         AdminHomeRedirectR    GET
```

#### Modify the `config/settings.yml` file

Add the following line, which sets the email address for an admin user account
that is created (and activated) in case no admin user exists:

```
admin: <your email address>
```

#### Modify the `Settings.hs` file

Append the following record to the `AppSettings` data type:

```haskell
    , appAdmin                  :: Text
```

Add this line to `instance FromJSON AppSettings` (ensure following
the same order as the properties appear in `settings.yml`):

```haskell
        appAdmin                  <- o .: "admin"
```

#### Modify the `config/models` file

Replace **all** of the file's content with the following `UserRole`
definition:

```
UserRole
    userId UserId
    roleName RoleName
    UniqueUserRole userId roleName
    deriving Typeable Show
```

#### Modify the `Models.hs` file

Add the following imports:

```haskell
import Roles
import LambdaCms.Core
```

#### Modify the `Application.hs` file

Add the following imports:

```haskell
import LambdaCms.Core
import LambdaCms.Core.Settings (generateUUID)
import qualified Network.Wai.Middleware.MethodOverridePost as MiddlewareMOP
```

And add the following function:

```haskell
getAdminHomeRedirectR :: Handler Html
getAdminHomeRedirectR = do
    redirect $ CoreAdminR AdminHomeR
```

Replace the `makeFoundation` function with the following code, so it will
create the `admin` user as provided in `settings.yml` and run all needed migrations:

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

In the function `makeApplication` replace this line:

```haskell
    return $ logWare $ defaultMiddlewaresNoLogging appPlain
```

With this line (adding a WAI middleware needed to make RESTful
forms work on older browsers):

```haskell
    return $ logWare $ MiddlewareMOP.methodOverridePost appPlain
```

## Create the `Roles.hs` file

Create the `Roles.hs` file (in the root directory of your
application) and add the following content to it:

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

## Modify the `Foundation.hs` file

Add the following imports:

```haskell
import qualified Data.Set                    as S
import qualified Network.Wai                 as W
import LambdaCms.Core
import Roles
```

Append the following record to the `App` data type:

```haskell
    , getLambdaCms   :: CoreAdmin
```

Change the implementation of `isAuthorized` (in `instance Yesod App`) to the
following, which allows fine-grained authorization based on `UserRoles`:

```haskell
isAuthorized theRoute _ = do
    mauthId <- maybeAuthId
    wai <- waiRequest
    y <- getYesod
    murs <- mapM getUserRoles mauthId
    return $ isAuthorizedTo y murs $ actionAllowedFor theRoute (W.requestMethod wai)
```

Change the implementation of `getAuthId` (in `instance YesodAuth App`) to:

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

In `instance YesodAuth App` replace:

```haskell
    loginDest _ = HomeR
    logoutDest _ = HomeR
```

With:

```haskell
    loginDest _ = CoreAdminR AdminHomeR
    logoutDest _ = AuthR LoginR
```

Add the following instance to allow `Unauthenticated` `GET` requests for the
`HomeR` route (likely to be `/`) and other common routes such as `/robots.txt`.
The last pattern forbids access to any unspecified routes.
It is in `actionAllowedFor` that you will setup permissions for the roles.

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
