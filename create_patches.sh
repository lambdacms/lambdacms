#!/bin/sh

# ./create_patches.sh <unpatched app root> <patched app root> <patch files go here>

UNPATCHED=$1
PATCHED=$2
KEEP=$3

REGULAR_FILES="Foundation.hs Application.hs Model.hs Roles.hs Settings.hs config/routes config/models config/settings.yml"
CABAL_FILE=`basename $1/*.cabal`


combined_file=$KEEP/all_patches_combined.patch
echo "" > $combined_file

for f in $REGULAR_FILES
  do target_file=$KEEP/`echo $f | sed "y/\//_/"`.patch
     diff -Nur $UNPATCHED/$f $PATCHED/$f > $target_file
     sed -i "s|$UNPATCHED|a|" $target_file
     sed -i "s|$PATCHED|b|" $target_file
     echo "diff -urN a/$f b/$f" >> $combined_file
     cat $target_file >> $combined_file
done

target_file=$KEEP/project_name.cabal.patch
diff -Nur $UNPATCHED/$CABAL_FILE $PATCHED/$CABAL_FILE > $target_file
sed -i "s|$UNPATCHED|a|" $target_file
sed -i "s|$PATCHED|b|" $target_file
sed -i "s|$CABAL_FILE|project_name.cabal|" $target_file
echo "diff -urN a/project_name.cabal b/project_name.cabal" >> $combined_file
cat $target_file >> $combined_file

