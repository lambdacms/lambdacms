#!/bin/bash

set -e

echo "==== CONFIG ===="
echo "Stack resolver:     " ${STACK_RESOLVER:=nightly-2015-07-09}
echo "Project name:       " ${PROJECT_NAME:=mysite}
echo "Database type:      " ${PROJECT_DB:=sqlite}  ;# other values: postgres, mysql
echo "Admin email address:" ${ADMIN_EMAIL:=admin@lambdacms.org}
echo "Copy unpatched:     " ${COPY_UNPATCHED:=no}

echo
echo "==== Generate Yesod app scaffold ===="
BASE_DIR=$PROJECT_NAME/$PROJECT_NAME-base
mkdir -p $BASE_DIR
cd $BASE_DIR
stack install yesod-bin --resolver $STACK_RESOLVER
yesod init -n $PROJECT_NAME -d $PROJECT_DB --bare

# Temporary step, will no longer be needed with LTS 3 is out
echo
echo "==== Bump some upperbounds ===="
cf=$PROJECT_NAME.cabal
sed -i "s%\(^ \+, yesod-static \+>= 1.4.0.3 \+&& <\) 1.5%\1 1.6%" $cf
sed -i "s%\(^ \+, persistent \+>= 2.0 \+&& <\) 2.2%\1 2.3%" $cf
sed -i "s%\(^ \+, persistent-postgres \+>= 2.1.1 \+&& <\) 2.2%\1 2.3%" $cf
sed -i "s%\(^ \+, persistent-sqlite \+>= 2.1.1 \+&& <\) 2.2%\1 2.3%" $cf
sed -i "s%\(^ \+, persistent-mysql \+>= 2.1.2 \+&& <\) 2.2%\1 2.3%" $cf

echo
echo "==== Generate a stack.yaml file ===="
(cd ..; stack init --resolver $STACK_RESOLVER)

echo
echo "==== Add non-Stackage dependencies to the stack.yaml file ===="
sed -i '/^extra-deps: \[\]$/d' ../stack.yaml
cat <<EOT >> ../stack.yaml
extra-deps:
- lambdacms-core-0.3.0.0
- friendly-time-0.4
- lists-0.4.2
- list-extras-0.4.1.4
EOT

# Maybe copy unpatched (helpful for updating the patch files)
if [ $COPY_UNPATCHED != "no" ]; then
  echo
  echo "==== Copy unpatched Yesod app ===="
  (cd ..; cp -R $PROJECT_NAME-base unpatched-$PROJECT_NAME-base)
fi

echo
echo "==== Patch the Yesod app into a minimal LambdaCms website ===="
TMP_CLONE=/tmp/lambdacms-clone-for-patches-`date +%s`
git clone --depth=1 https://github.com/lambdacms/lambdacms.git $TMP_CLONE
mv $PROJECT_NAME.cabal project_name.cabal
for f in $TMP_CLONE/yesod-scaffold-patches/*.patch; do patch -p1 < $f; done
mv project_name.cabal $PROJECT_NAME.cabal
rm -rf $TMP_CLONE

echo
echo "==== Setup the admin's email address ===="
NEW_SETTING="admin: \"_env:LAMBDACMS_ADMIN:$ADMIN_EMAIL\""
sed -i "/^admin:.*/c\\$NEW_SETTING" config/settings.yml

echo
echo "==== Done! ===="
echo "If all went well you are good to go!"
echo "To proceed you should first 'cd' into you project folder:"
echo "    cd $PROJECT_NAME"
echo "Build and test your project with the following commands:"
echo "    stack setup"
echo "    stack install"
echo "    stack test"
echo "Start a development server:"
echo "    stack exec -- yesod devel"
echo "And point your browser to the following URLs:"
echo "    http://localhost:3000/"
echo "    http://localhost:3000/admin/"