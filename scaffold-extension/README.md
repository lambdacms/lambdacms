scaffold-extension
==================

This repository contains a scaffolding script for creating new
[LambdaCms](http://lambdacms.org) extensions.

The files that are created by the script are found in this repository
as well. The "scaffolding variables" are surrounded by `%`-symbols.
The script, `create`, will replace the scaffolding variables, and
rename files based on its arguments.

Examples of basic extensions can be found in
[the code of the lambdacms.org website](https://github.com/lambdacms/lambdacms.org).
A more complex example is
[the `lambdacms-media` package`](https://github.com/lambdacms/lambdacms/tree/master/lambdacms-media).


## Creating the scaffold of a new extension

In the following commands we are going to create an extension for the
`Testimonial` content type, namespaced in the `MySite` module.

```bash
export TMP_CLONE=/tmp/lambdacms-clone-for-extension-scaffolding-`date +%s`
git clone --depth=1 https://github.com/lambdacms/lambdacms.git $TMP_CLONE
cp -r $TMP_CLONE/scaffold-extension .
rm -rf $TMP_CLONE
cd scaffold-extension
```

Here we clone it into the `mysite-post` folder, hinting that we will
create a `testimonial` extension to a base app called `mysite`.

Pick a module and an extension name to use when creating the extension
scaffold. In this example we name the extension model **`Testimonial`**
to be part of the **`MySite`** module (often the same as the module of
the "base application it will be part of).

```bash
./create_extension_scaffold.sh MySite Testimonial
```

**NOTE:** The arguments are case sensative. Since module names and
model types are capitalized in Haskell, you should always provide
capitalized arguments.


You can now run `stack build` to see if the freshly scaffolded extension
compiles.


#### Installing the new extension in a base app and "wire it up"

After running the script your new extension is ready to be installed.
If your base app is running in a cabal sandbox (recommended) you can
install the newly created extension by (1) adding the extension's
package as a dependency to your base app's cabal file, and (2)
adding the extension's folder as a source to the base app's `stack.yaml`
file.

To further "wire up" the extension into the base app, please refer to the
[README of lambdacms-media](https://github.com/lambdacms/lambdacms/tree/master/lambdacms-media).
It explains the further steps in detail.




# License

All code in this repository is released under the MIT license, as specified in the
[LICENSE file](https://github.com/lambdacms/lambdacms-extension-scaffold/blob/master/LICENSE).
