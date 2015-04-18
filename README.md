lambdacms-patches
=================

Patch set to install [lambdacms-core](https://github.com/lambdacms/lambdacms-core)
into a freshly init'ed [Yesod](http://yesodweb.com) application.

The patch set in this repository is tested with: **`yesod-bin v1.4.7`**


## Applying the patch set

First the patches need to be downloaded by cloning the repository, we do so in
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


## Patching a single file

It's also possible to patch just a single file (or a couple).
All `.patch` files except `all_patches_combined.patch` just patch a single file.
To patch `Foundation.hs` for instance, use the following command:

```bash
patch -p1 < Foundation.hs.patch
```


# Updating this patch set

Steps to update the patch set.

1.  Follow the [LambdaCms' installation guide](https://github.com/lambdacms/lambdacms-core/blob/master/README.md),
    up to the point where the patches need to be applied
2.  Run `(cd ..; cp -R $PROJECT_NAME unpatched-$PROJECT_NAME)` to save the unpatched version
3.  Apply all patches from thiis repo, except `lambdacms.patch`, one-by-one
4.  Take note of any patch chunks that got rejected
5.  Manually apply chunks that got rejected
6.  Get it to compile, and test if all works properly
7.  Create a new patch set with the `create_patches.sh` script
8.  Use a mult-file diff tool to see if the new patch set looks good
9.  Copy the new patch set's files over the files in this repository
10. Commit the changes
11. Make a new tag with the version number of `yesod-bin` (the scaffolder), e.g. `git tag patches-for-yesod-bin-1.4.7`
12. Push it with `git push origin --tags`


# License

All code in this repository is released under the MIT license, as specified
in the [LICENSE file](https://github.com/lambdacms/lambdacms-core/blob/master/LICENSE).
