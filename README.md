lambdacms-media
===============

This is an extension for [LambdaCms](http://lambdacms.org) which allows admins to manage media files.

**NOTE:** At this point the functionality provided by this extension is very basic. Pull request adding features are most welcome.


# Installing

LambdaCms extensions come as plain Haskell packages and need to be added to the
project's `.cabal` file like any other package dependency.

The following guide expects a newly initialized LambdaCms base application.
When you have an existing base app this guide show still be easy to follow.

In the `library` section of your base application's `.cabal` file append the following
line to `build-depends`:

```
                , lambdacms-media               >= 0.2.0      && < 0.3
```

The media extension's admin section needs to be mounted in the base app's
router, therefor add the following line to your `config/routes` file:

```
/admin/media  MediaAdminSubR       MediaAdmin   getLambdaCmsMedia
```

To `Application.hs` add `import LambdaCms.Media` and the following line:

```haskell
...
    let getLambdaCms = CoreAdmin
        getLambdaCmsMedia = MediaAdmin  -- add this line
        mkFoundation appConnPool = App {..}
...
```

The procede by including the `migrateLambdaCmsMedia` function to `Application.hs`
as shown in this snippet:

```haskell
...
    runLoggingT
        (runSqlPool (mapM_ runMigration [migrateAll, migrateLambdaCmsCore, migrateLambdaCmsMedia]) pool)
        (messageLoggerSource theFoundation appLogger)
...
```


To `Foundation.hs` also add `import LambdaCms.Media` and the following two lines:

```haskell
...
    , getLambdaCms   :: CoreAdmin
    , getLambdaCmsMedia :: MediaAdmin  -- add this line
    }
...
    adminMenu = (defaultCoreAdminMenu CoreAdminR)
                ++ (defaultMediaAdminMenu MediaAdminR)  -- add this line
    renderLanguages _ = ["en", "nl"]
...
```

The last line hooks the media admin section into the admin menu.

Finally the following instance needs to be defined in `Foundation.hs`:

```haskell
instance LambdaCmsMedia App where
  mediaR       = MediaAdminSubR
  staticDir y  = appStaticDir $ appSettings y
  staticRoot _ = "/static"
```


That's it! You can now `cabal install` the new dependency and run
`yesod devel` to test drive the freshly installed extension.



# License

All code in this repository is released under the MIT license, as specified
in the [LICENSE file](https://github.com/lambdacms/lambdacms-core/blob/master/LICENSE).


