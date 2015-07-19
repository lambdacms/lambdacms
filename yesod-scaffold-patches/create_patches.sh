#!/bin/sh

if [ $# -ne 3 ]; then
  echo "Usage: $0 <unpatched app root> <patched app root> <patch files go here>"
  exit 1
fi

UNPATCHED=$1
PATCHED=$2
KEEP=$3

REGULAR_FILES="Foundation.hs Application.hs Model.hs Roles.hs Settings.hs config/routes config/models config/settings.yml stack.yaml"
CABAL_FILE=`basename $UNPATCHED/*.cabal`

# regular files
for f in $REGULAR_FILES; do
  target_file=$KEEP/`echo $f | sed "y/\//_/"`.patch
  diff -Nur $UNPATCHED/$f $PATCHED/$f > $target_file
  sed -i "s|$UNPATCHED|a|" $target_file
  sed -i "s|$PATCHED|b|" $target_file
done

# cabal file
target_file=$KEEP/project_name.cabal.patch
diff -Nur $UNPATCHED/$CABAL_FILE $PATCHED/$CABAL_FILE > $target_file
sed -i "s|$UNPATCHED|a|" $target_file
sed -i "s|$PATCHED|b|" $target_file
sed -i "s|$CABAL_FILE|project_name.cabal|" $target_file

echo "Done."
