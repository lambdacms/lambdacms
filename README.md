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
git clone git@github.com:lambdacms/lambdacms-extension-scaffold.git mysite-testimonials
```

Here we clone it into the `mysite-post` folder, indicating that we will
create a testimonials extension to a base app called `mysite`.

Pick a module and an extension name to use when creating the extension.
In this example we name the extension module **`Testimonial`** for the
**`MySite`** base app module. Now run the `./create` command.

```bash
./create MySite Testimonial
```

**NOTE:** The arguments are case sensative. Since module names and
datatypes are capitalized in Haskell, you should always provide
capitalized arguments.

After running the create script your new extension is ready to be installed.

```bash
cabal install
```

The `create` script is not longer of use; just delete it with `rm create`.
In case you want to publish your extension, you probably want to edit your
extension's `.cabal` file setting things like author and license, and
procede by creating a repository for your newly created extension.


# License

All code in this repository is released under the MIT license, as specified in the
[LICENSE file](https://github.com/lambdacms/lambdacms-extension-scaffold/blob/master/LICENSE).
