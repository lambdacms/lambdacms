

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


# Setting up a site with LambdaCms

This guides you through the steps of setting up a site with LambdaCms.


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
  * Debian/Ubuntu: `libpq-dev`
  * CentOS/Fedora/RHEL: `postgresql-devel`
  * Homebrew (OSX): `postgres`
* For Mysql:
  * Debian/Ubuntu: `libmysqlclient-dev`
  * CentOS/Fedora/RHEL: `mysql-devel`
  * Homebrew (OSX): `mysql`
* For Sqlite
  * Debian/Ubuntu: `libsqlite3-dev`
  * CentOS/Fedora/RHEL: `sqlite-devel`
  * Homebrew (OSX): `sqlite`

On other platforms these packages might have different names, but are
most likely available.

If you are going to use a database other than Sqlite, you also need
to install that.


### Initializing the base application

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


### Patching a fresly init'ed Yesod application to include `lambdacms-core`

To add `lambdacms-core` to a freshly initialized Yesod application a number
of files need to be edited. We have prepared a patch-set to simplify this
process into a couple of commands.

First we need to download the patches by cloning the repository, we do so in
`/tmp`. Then we apply the patches with the good old `patch` command.

Run the following from the root of your newly created Yesod project:

    (cd /tmp; git clone https://github.com/lambdacms/lambdacms-patches.git)
    patch -p1 < /tmp/lambdacms-patches/lambdacms.patch

Because the cabl file has a different name for each project
(i.e. `<project_name>.cabal`) the patch command will notice a patched file
is missing (the original is named `orig_project.cabal`).
When the patch command tries to patch this file you will be prompted for
the name of your projects cabal file, after providing the name it will
successfully complete patching.

After patching your Yesod project there is one thing left to do.
Edit `config/settings.yml` to uncomment the last line (shown below) and
insert a valid email address.

```yaml
#admin: "_env:LAMBDACMS_ADMIN:<your email address>"
```

By this you specify the email address of an initial administrator,
so you can log in to the admin inteface.
By default the application uses Mozilla's [Persona](https://persona.org)
to log in: the email address used to log in need to be registered with Persona.


### Alternatives to the patch set

There are two alternatives to usign the patch-set:

1. Patch files individually, how to do so is explained in the `lambdacms-patches`
   [README](https://github.com/lambdacms/lambdacms-patches/blob/master/README.md).
2. Follow the [Getting Started Manually](https://github.com/lambdacms/lambdacms-core/wiki/Getting-Started-Manually)
   guide on the wiki.


### Enjoy the result

After applying the patches `lambdacms-core` is installed in your Yesod application.
Run `cabal install` (possibly with `-j1`) to fetch and build the dependencies.
Then run the development server.

    yesod devel

Now point your browser to `http://localhost:3000/admin` and you will be
prompted to login.  The setup as described above has selected Mozilla's
Persona as the only means of authentication.  In `config/settings.yml`
you have provided an email address for the admin user that is created
if no users exist. If this email address is known to Mozilla Persona
then you can procede to log in.


# License

All code in this repository is released under the MIT license, as specified
in the [LICENSE file](https://github.com/lambdacms/lambdacms-core/blob/master/LICENSE).


