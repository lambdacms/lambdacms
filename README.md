# LambdaCms extension boilerplate

## Create an extension from boilerplate

To create a new extension download this repo (or clone and remove the .git folder).

Pick a package and extension name to use. In the following example we will create a **Page** extension for the package **LambdaCms**. To generate the extension a create script is present within the directory, to use this run to following command:

```bash
./create LambdaCms Page
```

*note that the arguments are given case sensative. Because packages and datatypes in Haskell always start with an uppercased character both arguments should always start uppercased.*

After running the create script your new extension is ready to be installed, to install run:

```bash
cabal install
```

## Use extension with a LambdaCms website

*TODO*
