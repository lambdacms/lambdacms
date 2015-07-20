

```
                           ,                     _
                          /   _, _   /  _/ _,   / ) _  _,
                         (__ (/ //) () (/ (/   (__ //)_)

                   developer friendly :: type safe :: performant
```


# 1. Rationale &nbsp;[![Build Status](https://travis-ci.org/lambdacms/lambdacms.svg?branch=master)](https://travis-ci.org/lambdacms/lambdacms)

LambdaCms is a set of packaged libraries —containing subsites for the
[Yesod application framework](http://www.yesodweb.com)— which allow rapid
development of robust and highly performant websites with content management
functionality.

The `lambdacms-*` packages each provide some functionality and can depend
on eachother by specifying them as dependencies in their `.cabal` file.
The only mandatory package is `lambdacms-core`, it provides
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


# 2. Setting up a site with LambdaCms

This section walk through the steps of setting up a site with LambdaCms.

**NOTE:** We're currently in the process using
[`stack`](https://github.com/commercialhaskell/stack), and upgrading to
GHC 7.10. This means we will use Stackage's `nightly-2015-07-09` package
set until LTS 3 is released.


### Prerequisites

You need to be reasonably acquinted with Haskell in order to follow
along with this guide. To learn basic Haskell skills we recommend
Brent Yorgey's excellent
[Introduction to Haskell](http://www.seas.upenn.edu/~cis194/spring13)
course.

Besides Haskell you need to be somewhat familliar with:

* the web technologies (HTTP, HTML, CSS, JS, REST),
* RDBMS/SQL (LambdaCms makes use of a relational database), and
* the Yesod web application framework (for which an
* [awesome book](http://yesodweb.com/book) exists).


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

* For Sqlite (required for building the test suite)

  * Debian/Ubuntu: `libsqlite3-dev`
  * CentOS/Fedora/RHEL: `sqlite-devel`
  * Homebrew (OSX): `sqlite`

On other platforms these packages might have different names, but are
most likely available.

If you are going to use a database other than Sqlite (which directly writes
to a file), you need to have a database accessible from where you run your
site. This means you might have to install and setup a database server locally.


### Create a project folder

Choose a name for your project.  In below we choose `mysite`, which you
probably want to change. Make sure to choose a valid unix file name
to avoid naming issues.  Now create a directory for your project and
`cd` into it, by running the following commands:

```bash
export PROJECT_NAME=mysite; mkdir $PROJECT_NAME; cd $PROJECT_NAME
```


### Initializing the base application

First we need to install the `yesod` command, this command requires a
lot of dependent packages to be downloaded and build (may a while).

```bash
stack install yesod-bin --resolver nightly-2015-07-09
```

With the following command you create a "scaffolded" Yesod application.
The command is interactive; you need to supply some configuration values.
Pick the database of your choice, and choose a project name:

```bash
yesod init -n $PROJECT_NAME --bare
```

If you have chosen a database other than Sqlite, you need to create a
database and a sufficiently priviledged database user, and set these
credentials in the `config/setting.yml` file.

Now we will create a `stack.yaml` file for this project which specifies
the nightly snapshot we would like to use.

```
stack init --resolver nightly-2015-07-09
```

**NOTE:** This command complains that the some version constraints in
the `$PROJECT_NAME.cabal` file are too strict. Please raise the
upper bounds of these dependencies manually. **This step may be
removed once LTS 3 is out.**


This installs all dependencies and builds the scaffoled application
(may take a while):

```bash
stack install
```

When you experience problems during builds, while using LTS `3.x`,
we consider this a bug. Please
[raise an issue](https://github.com/lambdacms/lambdacms-core/issues).


### Testing the scaffoled Yesod app

The following commands will run your scaffolded Yesod application
in development mode.

```bash
stack exec yesod devel
```

Now fire some requests at it by pointing your browser to:
[`http://localhost:3000`](http://localhost:3000)

If all went well you are now ready to add LambdaCms to your app.


### Patching a freshly init'ed Yesod app scaffold to use `lambdacms-core`

To add `lambdacms-core` to a fresh Yesod application scaffold, a number
of files need to be edited. We use some patch files to automate this
process to a couple of simple shell commands.

Run the following commands from the root of your Yesod application scaffold.
It first downloads the patches by cloning the repository in to the `/tmp`
directory. Then the patches are applied with the good old `patch` command.

```bash
export TMP_CLONE=/tmp/lambdacms-clone-for-patches-`date +%s`
git clone --depth=1 https://github.com/lambdacms/lambdacms.git $TMP_CLONE
mv $PROJECT_NAME.cabal project_name.cabal
for f in $TMP_CLONE/yesod-scaffold-patches/*.patch; do patch -p1 < $f; done
mv project_name.cabal $PROJECT_NAME.cabal
rm -rf $TMP_CLONE
```

If any patches (partly) fail: try to fix it manually editing the files in
question. If this happens while closely following this README, then please
[open an issue](https://github.com/lambdacms/lambdacms/issues).


You could patch the files individually by manually unrolling the `for` loop
in the commands above.


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
Run `stack install` again to rebuild and install with the patches.

Then start the development server, which automatically recompiles when files
have changed.

    stack exec yesod devel

Point your browser to
[`http://localhost:3000/admin`](http://localhost:3000/admin) and you will be
prompted to login.  The setup as described above has selected Mozilla's
Persona as the only means of authentication.  In `config/settings.yml`
you have provided an email address for the admin user that is created
if no users exist. If this email address is known to Mozilla Persona
then you can procede to log in.


# 3. Add a LambdaCms "extensions" to your base application

LambdaCms' extension system is one of its core strengths:
it allows a developer to extend the site with packages while ensuring
full type safety.

Please refer to `lambdacms-media`'s
[README](https://github.com/lambdacms/lambdacms/blob/master/lambdacms-media/README.md)
for installation instructions. It also explains how other LambdaCms extensions
may incorporate media items managed by the `lambdacms-media` extension.

Or of course create your own extension...


# 4. Creating your own extensions

In order to make the code base of your website as modular as possible,
we recommend packaging functionality into LambdaCms extensions.
This allows functionality to be shared as a library.

Since this takes a bit of boiler plate, we have released a well documented
[extension scaffold script](https://github.com/lambdacms/lambdacms/blob/master/scaffold-extension)
that should get you started.


# 5. Examples

We plan to have more reference material online soon.

Currently the best place to have a look is
[the code for the lambdacms.org website](https://github.com/lambdacms/lambdacms.org).
It shows how a "base" application is extended with `lambdacms-core` and
lambdacms.org-specific extensions.



# License

All code in this repository is released under the MIT license, as specified
in the [LICENSE file](https://github.com/lambdacms/lambdacms/blob/master/LICENSE).
