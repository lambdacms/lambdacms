lambdacms-extension-scaffold
============================

This repository contains a scaffold for creating new
[LambdaCms](http://lambdacms.org) extensions.
The scaffold contains variables, surrounded by `%`-symbols.

This repository also contains a script, `create`, that will replace
the scaffold variables, and rename files based on its arguments.


## Creating an extension

To create a new extension download this repo (or clone and remove the
`.git` folder), for instance:

```bash
git clone git@github.com:lambdacms/lambdacms-extension-scaffold.git mysite-testimonial
```

Here we clone it into the `mysite-post` folder, hinting that we will
create a `testimonial` extension to a base app called `mysite`.

Pick a module and an extension name to use when creating the extension.
In this example we name the extension module **`Testimonial`** for the
**`MySite`** base app module. Now run the `./create` command.

```bash
./create MySite Testimonial
```

**NOTE:** The arguments are case sensative. Since module names and
datatypes are capitalized in Haskell, you should always provide
capitalized arguments.

The `create` script is no longer of use; just delete it with `rm create`.
In case you want to publish your extension, you probably want to edit your
extension's `.cabal` file setting things like author and license, and
procede by creating a repository for your newly created extension.


#### Installing the new extension in a base app and "wire it up"

After running the create script your new extension is ready to be installed.
If your base app is running in a cabal sandbox (recommended) you can
install the newly created extension by (1) adding the extension's
package as a dependency to your base app's cabal file, and (2)
adding the extension's folder as a source to the base app's sandbox.

For the second step run the following from the base app's folder:

```bash
cabal sandbox add-source ../mysite-testimonial
```

After which you can run a `cabal install` to see if everything worked.

To further "wire up" the extension into the base app, please refer to
the [README of lambdacms-media](https://github.com/lambdacms/lambdacms-media).
It explains the further steps in detail.




# License

All code in this repository is released under the MIT license, as specified in the
[LICENSE file](https://github.com/lambdacms/lambdacms-extension-scaffold/blob/master/LICENSE).
