lambdacms-patches
=================

Patch set to install [lambdacms-core](https://github.com/lambdacms/lambdacms-core)
into a freshly init'ed [Yesod](http://yesodweb.com) application.

The patch set in this repository is tested with: **`yesod-bin v1.4.3.3`**


## Applying the patch set

First the patches need to be downloaded by cloning the repository, we do so in
`/tmp`. Then we apply the patches with the good old `patch` command.

Run the following from the root of your newly created Yesod project:

```bash
(cd /tmp; git clone https://github.com/lambdacms/lambdacms-patches.git)
patch -p1 < /tmp/lambdacms-patches/lambdacms.patch
```

Because the cabl file has a different name for each project
(i.e. `<project_name>.cabal`) the patch command will notice a patched file
is missing (we named it `project_name.cabal`).
When the patch command tries to patch this file you will be prompted for
the name of your projects cabal file, after providing the name it will
successfully complete patching.


## Patching a single file

It's also possible to patch just a single file (or a couple).
All patch files except `lambdacms.patch` just patch a single file.
To patch `Foundation.hs` for instance, use the following command:

```bash
patch -p1 < foundation.patch
```

The single file patches are named based on the file they patch and the directory this file exists in. All file names are lowercased and the extensions have been removed, directories are added underscore separated. The following files can be patched separtely:

* `Foundation.hs`
* `Application.hs`
* `Model.hs`
* `Roles.hs`
* `Settings.hs`
* `config/routes`
* `config/models`
* `config/settings.yml`
* `<project_name>.cabal`


# License

All code in this repository is released under the MIT license, as specified
in the [LICENSE file](https://github.com/lambdacms/lambdacms-core/blob/master/LICENSE).
