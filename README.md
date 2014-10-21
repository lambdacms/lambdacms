


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

In this installation guide we expect the project to named `ponycms`, obviously
you want to replace that with the name of your project (or keep it in case you
are just testing it out and want the convenience of blind copy-pasting the
instructions that follow).


## Haskell & co

Make sure to have **GHC** 7.6+, **cabal-install** 1.18+, **happy**, **alex**
and **yesod-bin** 1.4+ installed, and their binaries available to your shell's `$PATH`.

To get GHC and Cabal ready we used Yann Esposito's [Safer Haskell Install](http://yannesposito.com/Scratch/en/blog/Safer-Haskell-Install/)
script. This script can be used for Linux and OSX and will install GHC and Cabal.
It also enables the usage of stackage to make sure all libraries which are
installed are compatible. We are currently using the following stackage repository:

    remote-repo: stackage:http://www.stackage.org/stackage/1ee16e7b56ea1e1ac4e15ce7a1cc72018b2117c1

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
name you picked, we used `ponycms`. After scaffolding move into the project folder.

    yesod init
    cd ponycms

If you have chosen a database other then Sqlite, then configure it in one of the `.yml` files
the can be found in the `config/` directory.  Also make sure the database can be accessed with
the given configuration.

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
    cd ../ponycms

Add the following line to the `build-depends` of the `library` section of the
`ponycms.cabal` file (the version bounds may need to be raised):

                     , lambdacms-core                >= 0.0.7      && < 0.1

Add the following lines to `config/routes`:

    /admin/core         CoreR                  Core        getLambdaCms
    /admin/auth         AuthR                  Auth        getAuth
    /admin              AdminHomeRedirectR     GET

Add the following lines to the imports of both `Foundation.hs` *and* `Application.hs`:

    import LambdaCms

And to `Foundation.hs` add the lines:

    instance LambdaCmsAdmin App
        maybeAuth' = maybeAuth
        maybeAuthId' = maybeAuthId
        authLoginDest _ = AuthR LoginR

And change the following lines in `Foundation.hs`:

    loginDest _ = HomeR
    logoutDest _ = HomeR

to:

   loginDest _ = CoreR AdminHomeR
   logoutDest _ = CoreR AdminHomeR

In `Application.hs` the following lines should be added:

    getAdminHomeRedirectR :: Handler Html
    getAdminHomeRedirectR = do
        redirect $ CoreR AdminHomeR

**NOTE:** From here it is pretty much work in progress... You're on your own, good luck.

Further steps to add:

* Wire it up (`getLambdaCms` to the App, etc.)
* Run `caball install` and `yesod devel`
* Test


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
