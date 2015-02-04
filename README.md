

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

Using this guide you will create a CMS website named `mysite`.
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
, **yesod-bin** 1.4.3.3+ installed, and their binaries available from your
shell's `$PATH`.

To check that you are good to go, you can use these commands.

    ghc -V
    cabal -V
    happy -V
    alex -V
    yesod version

In case you are **not** good to go, you may want to follow the
[installation guide on the Stackage website](http://www.stackage.org/install)
which provides instructions for all dependencies except `yesod-bin`.

Once you meet all the requirements except `yesod-bin`, install it.

    cabal install "yesod-bin >= 1.4.3.3"


### Required non-Haskell dependencies

For the connection with the database, Haskell libraries typically compile
against non-Haskell libraries. One of the following libraries needs to be
available:

* For Postgres:
 * Ubuntu: `libpq-dev`
 * Homebrew on OSX: `postgres`
* For Mysql:
  * Ubuntu: `libmysqlclient-dev`
  * Homebrew on OSX: `mysql`
* For Sqlite
  * Ubuntu: `libsqlite3-dev`
  * Homebrew on OSX: `sqlite`

On other platforms these packages might have different names, but are
most likely available.

If you are going to use a database other than Sqlite, you also need
to install that.

### Create the base application

With the following command you create a "scaffolded" Yesod application.
The command is interactive; you need to supply some configuration values.
Pick the database of your choice, and choose a project name

    yesod init

After scaffolding `cd` into the project folder.

If you have chosen a database other than Sqlite, you need to create a
database and a sufficiently priviledged user, and supply the
credentials to the `config/setting.yml` file.


### Using LTS Haskell

To avoid spending too much time on build issues we use and recommend
[LTS Haskell](https://github.com/fpco/lts-haskell#readme).

Currently we develop and test LambdaCms only against the lastest
LTS Haskell release. As minor releases of LTS Haskell should never
contain breaking changes we only provide the major release number,
thereby automatically using the most recent minor release in that
series.

Run the following commands from within your project's root folder,
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

LambdaCms is on Hackage! Install with: `cabal install lambdacms-core`

In the following sub-sections we explain how to install `lambdacms-core` into
the base application.  Much of what we show here can be accomplished in
many different ways, in this guide we merely provide a way to get you started.

#### Patching a new Yesod application

To setup a new LambdaCms website the easy way we created patch files to convert
a new Yesod application to a LambdaCms website. Those patches can be found in
[lambdamcs-patches](https://github.com/lambdacms/lambdacms-patches). Either clone
the repository or copy only the required patches to your local environment and
run the following command (replace `/path/to` with the actual path to the patch
file):

    patch -p1 < /path/to/lambdacms.patch

This patches all files at the same time. Documentation about how to patch files
individually can be found in the `lambdacms-patches`
[README](https://github.com/lambdacms/lambdacms-patches/blob/master/README.md).

To manually add LambdaCms to a Yesod application follow the steps below.

#### Modify the `.cabal` file

First add `lambdacms-core` to the dependencies list of the `.cabal`
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

Replace **all** of the file's content with:

```
/static StaticR Static appStatic

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/ HomeR GET

/admin/auth        AuthR                 Auth            getAuth
/admin/core        CoreAdminR            CoreAdmin       getLambdaCms
/admin             AdminHomeRedirectR    GET
```

#### Modify Settings

There are two files to modify here. One is `config/settings.yml` and the the other is `Settings.hs`.

In `config/settings.yml`, add the following line to the bottom of the file
```
admin: "_env:LAMBDACMS_ADMIN:<your email address>"
```

Then, in `Settings.hs`, append the following record to the `AppSettings` data type:

```haskell
    , appAdmin                  :: Text
```

In `instance FromJSON AppSettings`, find this line:

Add the following line just before the line containing `return AppSettings {..}`:

```haskell
        appAdmin                  <- o .: "admin"
```

#### Modify the `config/models` file

Replace **all** of the file's content with the following `UserRole` definition:

```
UserRole
    userId UserId
    roleName RoleName
    UniqueUserRole userId roleName
    deriving Typeable Show
```

#### Modify the `Model.hs` file

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
import Network.Wai.Middleware.MethodOverridePost
```

Add the following function:

```haskell
getAdminHomeRedirectR :: Handler Html
getAdminHomeRedirectR = do
    redirect $ CoreAdminR AdminHomeR
```

In the `makeFoundation` function, add this line to beginning:

```haskell
let getLambdaCms = CoreAdmin
```

Then, find this code:

```haskell
    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool
```

And replace it with:

```haskell
    -- Perform database migration using our application's logging settings.
    let theFoundation = mkFoundation pool
    runLoggingT
        (runSqlPool (mapM_ runMigration [migrateAll, migrateLambdaCmsCore]) pool)
        (messageLoggerSource theFoundation appLogger)

    -- Create a user if no user exists yet
    let admin = appAdmin appSettings
    madmin <- runSqlPool (getBy (UniqueEmail admin)) pool
    case madmin of
        Nothing -> do
            timeNow <- getCurrentTime
            uuid <- generateUUID
            flip runSqlPool pool $ do
                uid <- insert User { userIdent     = uuid
                                   , userPassword  = Nothing
                                   , userName      = takeWhile (/= '@') admin
                                   , userEmail     = admin
                                   , userActive    = True
                                   , userToken     = Nothing
                                   , userCreatedAt = timeNow
                                   , userLastLogin = Nothing
                                   , userDeletedAt = Nothing
                                   }
                -- assign all roles to the first user
                mapM_ (insert_ . UserRole uid) [minBound .. maxBound]
        _ -> return ()

    -- Return the foundation
    return theFoundation
```


In the function `makeApplication` replace this line:

```haskell
    return $ logWare $ defaultMiddlewaresNoLogging appPlain
```

With this line (adding a WAI middleware is needed to make RESTful
forms work on older browsers):

```haskell
    return $ logWare $ methodOverridePost appPlain
```

## Create the `Roles.hs` file

Create the `Roles.hs` file (in the root directory of your
application) and add the following content to it:

```haskell
module Roles where

import ClassyPrelude.Yesod

data RoleName = Admin
              | Blogger
              deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable)

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
    isAuthorized theRoute _ =
        case theRoute of
            (StaticR _)                   -> return Authorized
            (CoreAdminR (AdminStaticR _)) -> return Authorized
            _                             -> do
                mauthId <- maybeAuthId
                wai     <- waiRequest
                y       <- getYesod
                murs    <- mapM getUserRoles mauthId
                return $ isAuthorizedTo y murs $ actionAllowedFor theRoute (W.requestMethod wai)

```

Change the implementation of `getAuthId` (in `instance YesodAuth App`) to:

```haskell
    getAuthId = getLambdaCmsAuthId
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

And add:

```haskell
    authLayout = adminAuthLayout
```


Add the following instance to allow `Unauthenticated` `GET` requests for the
`HomeR` route (likely to be `/`) and other common routes such as `/robots.txt`.
The last pattern allows access to any unspecified routes for just Admins -- which is a role defined in Roles.hs.
It is in `actionAllowedFor` that you will setup permissions for the roles.

```haskell
instance LambdaCmsAdmin App where
    type Roles App = RoleName

    actionAllowedFor (FaviconR) "GET" = Unauthenticated
    actionAllowedFor (RobotsR)  "GET" = Unauthenticated
    actionAllowedFor (HomeR)    "GET" = Unauthenticated
    actionAllowedFor (AuthR _)  _     = Unauthenticated
    actionAllowedFor _          _     = Roles $ S.fromList [Admin]

    coreR = CoreAdminR
    authR = AuthR
    masterHomeR = HomeR

    -- cache user roles to reduce the amount of DB calls
    getUserRoles userId = cachedBy cacheKey . fmap toRoleSet . runDB $ selectList [UserRoleUserId ==. userId] []
        where
            cacheKey = encodeUtf8 $ toPathPiece userId
            toRoleSet = S.fromList . map (userRoleRoleName . entityVal)

    setUserRoles userId rs = runDB $ do
        deleteWhere [UserRoleUserId ==. userId]
        mapM_ (insert_ . UserRole userId) $ S.toList rs

    adminMenu =  (defaultCoreAdminMenu CoreAdminR)
    renderLanguages _ = ["en", "nl"]

    mayAssignRoles = do
        authId <- requireAuthId
        roles <- getUserRoles authId
        return $ isAdmin roles
```

#### Add an `isAdmin` function to the bottom of `Foundation.hs`

Remember that the `Admin` role is defined in Roles.hs
```haskell
isAdmin :: S.Set RoleName -> Bool
isAdmin = S.member Admin
```


### Give it a try

LambdaCms should now be installed. You can give it a try.

    yesod devel

Now point your browser to `http://localhost:3000/admin` and you will be
prompted to login.  The setup as described above has selected Mozilla's
Persona as the only means of authentication.  In `config/settings.yml`
you have provided an email address for the admin user that is created
if no users exist. If this email address is known to Mozilla Persona
then you can procede to log in.
