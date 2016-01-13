# Patching Yesod application

To setup a new LambdaCms website the easy way we created patch files to convert
a new Yesod application to a LambdaCms website.  Those patches can be found in
[lambdamcs-patches](https://github.com/lambdacms/lambdacms-patches).  Either
clone the repository or copy only the required patches to your local environment
and run the following command (replace `/path/to` with the actual path to the
patch file):

    patch -p1 < /path/to/lambdacms.patch

This patches all files at the same time.  Documentation about how to patch files
individually can be found in the `lambdacms-patches`
[README](https://github.com/lambdacms/lambdacms-patches/blob/master/README.md).

To manually add LambdaCms to a Yesod application follow the steps below.

## Modify the `.cabal` file

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

## Modify the `config/routes` file

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

## Modify Settings

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

## Modify the `config/models` file

Replace **all** of the file's content with the following `UserRole` definition:

```
UserRole
    userId UserId
    roleName RoleName
    UniqueUserRole userId roleName
    deriving Typeable Show
```

Note, that **LambdaCms** provides its own `User` model, which automatically
imported by `import LambdaCms.Core`.  You can find source at
[`lambdacms/lambdacms-core/config/models`](https://github.com/lambdacms/lambdacms/blob/master/lambdacms-core/config/models).

## Modify the `Model.hs` file

Add the following imports:

```haskell
import Roles
import LambdaCms.Core
```

## Modify the `Application.hs` file

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

Don't forget to update `userPassword` to `Just` value if your auth plugin is
based of login/password authorization which is not the case for **Mozilla's
Persona**, e.g. if you use `authEmail` from `Yesod.Auth.Email` you need to store
salted password:

```
-- import Yesod.Auth.Email (saltPass)

…

            timeNow <- getCurrentTime
            uuid <- generateUUID
            pass <- saltPass "my_secret_password!"
            flip runSqlPool pool $ do
                uid <- insert User { userIdent     = uuid
                                   , userPassword  = Just pass
…
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
                return $ isAuthorizedTo y murs $
                    actionAllowedFor theRoute (W.requestMethod wai)

```

Change the implementation of `maybeAuthId` (in `instance YesodAuth App`) to:

```haskell
    maybeAuthId = lambdaCmsMaybeAuthId
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


Add the following instance to allow unauthenticated `GET` requests for the
`HomeR` route (likely to be `/`) and other common routes such as `/robots.txt`.
The last pattern allows access to any unspecified routes for just Admins --
which is a role defined in Roles.hs.  It is in `actionAllowedFor` that you will
setup permissions for the roles.

```haskell
instance LambdaCmsAdmin App where
    type Roles App = RoleName

    actionAllowedFor (FaviconR) "GET" = AllowAll
    actionAllowedFor (RobotsR)  "GET" = AllowAll
    actionAllowedFor (HomeR)    "GET" = AllowAll
    actionAllowedFor (AuthR _)  _     = AllowAll
    actionAllowedFor _          _     = AllowRoles $ S.fromList [Admin]

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

## Add an `isAdmin` function to the bottom of `Foundation.hs`

Remember that the `Admin` role is defined in Roles.hs
```haskell
isAdmin :: S.Set RoleName -> Bool
isAdmin = S.member Admin
```


## Give it a try

LambdaCms should now be installed. You can give it a try.

    yesod devel

Now point your browser to `http://localhost:3000/admin` and you will be
prompted to login.  The setup as described above has selected Mozilla's
Persona as the only means of authentication.  In `config/settings.yml`
you have provided an email address for the admin user that is created
if no users exist. If this email address is known to Mozilla Persona
then you can procede to log in.
