#!/bin/sh


echo "Stack resolver:     " ${STACK_RESOLVER:=nightly-2015-07-09}
echo "Project name:       " ${PROJECT_NAME:=mysite}
echo "Database type:      " ${PROJECT_DB:=sqlite}  ;# other values: postgres, mysql
echo "Admin email address:" ${ADMIN_EMAIL:=admin@lambdacms.org}
echo "Copy unpatched:     " ${COPY_UNPATCHED:=no}

# Setup a new scaffolded Yesod app
mkdir $PROJECT_NAME
cd $PROJECT_NAME
stack install yesod-bin --resolver $STACK_RESOLVER
yesod init -n $PROJECT_NAME -d $PROJECT_DB --bare
stack init --resolver $STACK_RESOLVER

# Bump upperbounds of some dependencies (needed for nightly-2015-07-09)
cf=$PROJECT_NAME.cabal
sed -i "s%\(^ \+, yesod-static \+>= 1.4.0.3 \+&& <\) 1.7%\1 1.6%" $cf
sed -i "s%\(^ \+, persistent \+>= 2.0 \+&& <\) 2.2%\1 2.3%" $cf
sed -i "s%\(^ \+, persistent-postgres \+>= 2.1.1 \+&& <\) 2.2%\1 2.3%" $cf
sed -i "s%\(^ \+, persistent-sqlite \+>= 2.1.1 \+&& <\) 2.2%\1 2.3%" $cf
sed -i "s%\(^ \+, persistent-mysql \+>= 2.1.2 \+&& <\) 2.2%\1 2.3%" $cf

# Maybe copy unpatched
if [ $COPY_UNPATCHED -ne "no" ]; then
  (cd ..; cp -R $PROJECT_NAME unpatched-$PROJECT_NAME)
fi

# Patch the Yesod app into a minimal LambdaCms website
TMP_CLONE=/tmp/lambdacms-clone-for-patches-`date +%s`
git clone --depth=1 https://github.com/lambdacms/lambdacms.git $TMP_CLONE
mv $PROJECT_NAME.cabal project_name.cabal
for f in $TMP_CLONE/yesod-scaffold-patches/*.patch; do patch -p1 < $f; done
mv project_name.cabal $PROJECT_NAME.cabal
rm -rf $TMP_CLONE

# Setup the admin's email address
NEW_SETTING="admin: \"_env:LAMBDACMS_ADMIN:$ADMIN_EMAIL\""
sed -i "/^admin:.*/c\\$NEW_SETTING" config/settings.yml

# Build and run test suite
stack setup
stack install
stack test

# Try it yourself
echo "If all went well you are good to go!"
echo "Try running: stack exec -- yesod devel"
echo "And point your browser to the following urls:"
echo "    http://localhost:3000/"
echo "    http://localhost:3000/admin/"



