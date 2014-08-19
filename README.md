


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

Make sure to have **GHC** 7.6+, **cabal-install** 1.20+, **happy** and **alex**
installedi, and their binaries available to your shell's `$PATH`.

[Haskell Platform](https://www.haskell.org/platform/) provides a convenient way
to install these dependencies. Alternatively you can follow the "leaner" route
of installing Haskell by following
[these installation instructions](http://haskell-lang.org/downloads) for Linux and OSX.

To check that you are good to go:

    ghc -V
    cabal -V
    happy -V
    alex -V


## Yesod in a cabal sandbox

Setup a cabal sandbox for your master application.

    mkdir ponycms
    cd ponycms
    cabal update
    cabal sandbox init
    cabal install yesod-platform yesod-bin

Make sure `.cabal-sandbox/bin` is in the `$PATH`.

    export PATH=.cabal-sandbox/bin:$PATH

Scaffold the master application, this interactively takes some configuration values.
The `--bare` flag omits creating subdir.

    yesod init --bare

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
with the `cabal sandbox add-source` command.

    cd ..
    git clone git@github.com:lambdacms/lambdacms-core.git
    cd ponycms
    cabal sandbox add-source ../lambdacms-core

Add the following line to the `build-depends` of the `library` section of the
`ponycms.cabal` file (the version bounds may need to be raised):

                     , lambdacms-core                >= 0.0.7      && < 0.1

Add the following line to `config/routes`:

    /admin         CoreR      Core        getLambdaCms

Add the following line to the imports of both `Foundation.hs` *and* `Application.hs`:

    import LambdaCms

And to `Foundation.hs` add the line:

    instance LambdaCmsAdmin App

**NOTE:** From here it is pretty much work in progress... You're on your own, good luck.

Further steps to add:

* Wire it up (`getLambdaCms` to the App, etc.)
* Run `caball install` and `yesod devel`
* Test


# Links of interest

Obviously the [Yesod book](http://www.yesodweb.com/book) is a must read,
beyond that docs may sommetimes be scarse.
Therefore this collection of links that may shed some light on corners of Yesod
that are of particular interest when hacking on LambdaCms.

http://stackoverflow.com/questions/13055611/what-should-the-type-be-for-a-subsite-widget-that-can-be-used-in-a-master-site

http://www.yesodweb.com/book/wiki-chat-example

https://github.com/yesodweb/yesod/tree/master/yesod-auth (probably the most complex subsite example)
