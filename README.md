# LambdaCms Patches

To make the inital setup for a [LambdaCms](http://www.lambdacms.org) website a lot easier, the patches within this repository can be used.
Either clone this repository or copy the required patch file to your machine.

The current patch files are tested with the following Yesod version:

* 1.4.3.3

## Patching all in one

To patch a complete (new) Yesod instance to become a LambdaCms website the file `lambdacms.patch` should be used. Run the following command from with in the Yesod applications root directory (`/path/to` should be replaced with the actual path to the patch file):

```bash
patch -p1 < /path/to/lambdacms.patch
```

Because the name of the `.cabal` file is different for every project the patch command will notice a patched file is missing (we named our `.cabal` `patch.cabal`). When the patch command tries to patch this file you will be prompted for the name of your projects `.cabal` file and you should provide this to successfully finish the patching.

After patching your Yesod project there is one thing left to do. To successfully run the application with `yesod devel` you should edit `config/settings.yml` by uncommenting the last line and inserting a valid email address:

```yaml
#admin: "_env:LAMBDACMS_ADMIN:<your email address>"
```

## Patch a single file

It's also possible to patch just a single file (or a couple). All patch files except `lambdacms.patch` just patch a single file. To patch `Foundation.hs` for instance, use the following command:

```bash
patch -p1 < foundation.patch
```

The single file patches are named based on the file they patch and the directory this file exists in. All file names are lowercased and the extensions have been removed, directories are added underscore separated. The following files can be patched separtely:

* Foundation.hs
* Application.hs
* Model.hs
* Roles.hs
* Settings.hs
* config/routes
* config/models
* config/settings.yml
* {project}.cabal
