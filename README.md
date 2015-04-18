

```
                           ,                     _
                          /   _, _   /  _/ _,   / ) _  _,
                         (__ (/ //) () (/ (/   (__ //)_)

                   developer friendly :: type safe :: performant
```


# Rationale

LambdaCms is a set of packaged libraries —containing subsites for the
[Yesod application framework](http://www.yesodweb.com)— which allow rapid
development of robust and highly performant websites with content management
functionality.

The `lambdacms-*` packages each provide some functionality and can depend
on eachother as they depend on other packages.
The only mandatory package is `lambdacms-core` (this package), it provides
functionality that all other `lambdacms-*` packages depend on.

As mentioned, each `lambdacms-*` package contains a subsite which is
"mounted" in a standard Yesod application, which we will refer to as
"the base application" or simply "base".
Before a packaged subsite can be mounted, the package needs to be
included as a dependency to the base app's `.cabal` file.  After that
some glue code needs to be added to the base app, as explained below.

In the base app we have to:

* configure a database connection,
* organize the admin backend's menu,
* specify the authentication strategies for admins, and
* define admin user roles and their permissions.

In the base app we optionally may also:

* override default behavior,
* override UI texts,
* provide a means to send email notifications, and last but not least,
* write the themes so the website can actually be visited (recommended).


# Setting up a site with LambdaCms

This section walk through the steps of setting up a site with LambdaCms.


### Prerequisites

You need to be reasonably acquinted with Haskell in order to follow
along with this guide. To learn basic Haskell skills we recommend
Brent Yorgey's excellent
[Introduction to Haskell](http://www.seas.upenn.edu/~cis194/spring13)
course.

Besides Haskell you need to be somewhat familliar with:

* the web technologies (HTTP, HTML, CSS, JS, REST),
* RDBMS/SQL (LambdaCms makes use of a relational database), and
* the Yesod web application framework (for which an [awesome book](http://yesodweb.com/book) exists).


### Non-Haskell dependencies

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

If you are going to use a database other than Sqlite (which directly writes
to a file), you need to have a database accessible from where you run your
site. This means you might have to install and setup a database server locally.


### The tool chain

Make sure to have **GHC** 7.8.3+, **cabal-install** 1.20+, **happy** and **alex**
installed; and their binaries available from your shell's `$PATH`.

Use the following command to check your system meets the requirements:

```bash
for c in ghc cabal happy alex; do $c -V | head -n1; done
```

When good to go the output should be similar to:

```
The Glorious Glasgow Haskell Compilation System, version 7.8.4
cabal-install version 1.20.0.3
Happy Version 1.19.5 Copyright (c) 1993-1996 Andy Gill, Simon Marlow (c) 1997-2005 Simon Marlow
Alex version 3.1.4, (c) 2003 Chris Dornan and Simon Marlow
```

In case you are **not** good to go, you may want to follow the
[per operating system installation guides on the haskell.org website](https://www.haskell.org/downloads)
which provides instructions for installing the tool chain and setting
up your `$PATH`.


### Create a project folder

Choose a name for your project.  In below we chose `mysite`, which you
probably want to change. Make sure to choose a valid unix file name
to avoid naming issues.  Now create a directory for your project and
`cd` into it, by running the following commands:

```bash
export PROJECT_NAME=mysite; mkdir $PROJECT_NAME; cd $PROJECT_NAME
```


### Initialize a cabal sandbox

To avoid running into version conflicts with other Haskell projects
you might be working on from the same system, we setup a cabal sandbox.

From within your project's folder run the following commmand:

```bash
cabal sandbox init
```


### Using LTS Haskell

To avoid spending too much time on build issues we use and recommend
[LTS Haskell](https://github.com/fpco/lts-haskell#readme).

Currently we develop and test LambdaCms only against the `2.x`
LTS Haskell releases. As minor releases of LTS Haskell should never
contain breaking changes, you can safely use the latest release of
a major LTS version.

Run the following commands from within your project's folder,
to install the most recent LTS Haskell package set in the `2.x` series.

```bash
wget http://www.stackage.org/lts/2/cabal.config
cabal update
```


### Initializing the base application

First we need to install the `yesod` command, this command requires a
lot of dependent packages to be downloaded and build (may a while).
Run this from your project's folder:

```bash
cabal install yesod-bin
```

With the following command you create a "scaffolded" Yesod application.
The command is interactive; you need to supply some configuration values.
Pick the database of your choice, and choose a project name:

```bash
yesod init --bare
```

If you have chosen a database other than Sqlite, you need to create a
database and a sufficiently priviledged database user, and set these
credentials in the `config/setting.yml` file.

This installs all dependencies and builds the scaffoled application 
(may take a while):

```bash
cabal install -j --enable-tests --max-backjumps=-1 --reorder-goals
```

In case you experience problems with `cabal install` try changing
`-j` into `-j1` to prevents concurrent building, and/or simply retry
the command until you consistently run into the same error.

When you experience problems during builds, while using LTS `2.x`,
we consider this a bug. Please
[raise an issue](https://github.com/lambdacms/lambdacms-core/issues).


### Testing your Yesod app

The following commands will run your scaffolded Yesdo application
in development mode.

```bash
yesod devel
```

Now test it by pointing the browser to:
[`http://localhost:3000`](http://localhost:3000)

If all went well you are ready to add LambdaCms to your app.


### Patching a freshly init'ed Yesod app to use `lambdacms-core`

To add `lambdacms-core` to a freshly initialized Yesod application a number
of files need to be edited. We have prepared a patch-set to simplify this
process into a couple of commands.

First we need to download the patches by cloning the repository, we do so in
`/tmp`. Then we apply the patches with the good old `patch` command.

Run the following from the root of your newly created Yesod project:

```bash
(cd /tmp; git clone https://github.com/lambdacms/lambdacms-patches.git)
patch -p1 < /tmp/lambdacms-patches/all_patches_combined.patch
```

Because the cabal file has a different name for each project
(i.e. `<project_name>.cabal`) the patch command will notice a patched file
is missing (we named it `project_name.cabal`).
When the patch command tries to patch this file you will be prompted for
the name of your projects cabal file, after providing the name it will
successfully complete patching.


#### Alternatives to the patch set

There are two alternatives to using the patch set:

1. Patch files individually, how to do so is explained in the `lambdacms-patches`
   [README](https://github.com/lambdacms/lambdacms-patches/blob/master/README.md).
2. Follow the [Getting Started Manually](https://github.com/lambdacms/lambdacms-core/wiki/Getting-Started-Manually)
   guide on the wiki.


### Configure the initial administrator

By default the application uses Mozilla's [Persona](https://persona.org)
to log in: the email address used to log in need to be registered with Persona.
It is recommended to use an email address of a Persona account for
development as it simplifies logging in during development.

Edit `config/settings.yml` to insert a valid email address.

```yaml
admin: "_env:LAMBDACMS_ADMIN:<email address>"
```

Replace `<email address>` with the email address of an initial administrator
or developer, so the admin inteface can be accessed.


### Enjoy!

After applying the patches `lambdacms-core` is installed in your Yesod application.
Run `cabal install` (possibly with `-j1`) to fetch and build the dependencies.
Then run the development server.

    yesod devel

Now point your browser to
[`http://localhost:3000/admin`](http://localhost:3000/admin) and you will be
prompted to login.  The setup as described above has selected Mozilla's
Persona as the only means of authentication.  In `config/settings.yml`
you have provided an email address for the admin user that is created
if no users exist. If this email address is known to Mozilla Persona
then you can procede to log in.


## Adding a LambdaCms extension to your base application

LambdaCms' extension system is one of its core strengths:
it allows a developer to extend the site in
a with full type safety, ensuring a working and robust product.

To illustrate installing an extension to your base application we
look at `lambdacms-media`. Installing extensions boils down to
mostly the same steps.

See the `lambdacms-media`'s [README](https://github.com/lambdacms/lambdacms-media/blob/master/README.md)
for installation instructions. It also explains how other LambdaCms extensions
may incorporate media items managed by the `lambdacms-media` extension.


# License

All code in this repository is released under the MIT license, as specified
in the [LICENSE file](https://github.com/lambdacms/lambdacms-core/blob/master/LICENSE).


