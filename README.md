lambdacms-extension-scaffold
============================

This repository contains a scaffold for creating new
[LambdaCms](http://lambdacms.org) extensions.
The scaffold contains variables, surrounded by `%`-symbols.

This repository also contains a script, `create`, that will replace
the scaffold variables, and rename files based on its arguments.


## Creating an extension

To create a new extension download this repo (or clone and remove the
`.git` folder).

Pick a module and an extension name to use when creating the extension.
In the following example we will create a **`Testimonial`** extension
for the **`MySite`** module. Now run the `create` command.

```bash
./create MySite Testimonial
```

**NOTE:** The arguments are case sensative, and since module names and
datatypes in start with a capital in Haskell, you should always provide
two arguments that start with capitals.

After running the create script your new extension is ready to be installed.

```bash
cabal install
```

You can now procede by creating a repository for your newly created extension.
You might also want to remove the `create` script, as you no longer need it.


# License

All code in this repository is released under the MIT license, as specified in the
[LICENSE file](https://github.com/lambdacms/lambdacms-extension-scaffold/blob/master/LICENSE).
