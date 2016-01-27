#!/bin/bash

set -e

if [ $# -ne 2 ]; then
  echo "Usage: $0 <ModuleName> <ModelName>"
  echo "The ModuleName is the project that the extension is part of."
  echo "The ModelName is the name of the content type that this extension will provide."
  echo "Both should be CamelCased."
  exit
fi

# TODO: FIXME: "package" is actually a "module", replace all over.
package=$1
model=$2
lc_package=$(echo "$package" | tr '[:upper:]' '[:lower:]')
lc_model=$(echo "$model" | tr '[:upper:]' '[:lower:]')
lcc_model=$(echo "$model" | cut -c1-1 | tr '[:upper:]' '[:lower:]')$(echo "$model" | cut -c2-99)
# the `xargs` command "strips" whitespace off the ends of the string
lcs_model=$(echo "$model" | sed 's/\([A-Z][^A-Z]\)/ \1/g' | tr '[:upper:]' '[:lower:]' | xargs)
lc_combined="${lc_package}-${lc_model}"

files=($(find PACKAGE -type f))
files+=($(find config -type f))
files+=($(find templates -type f))
files+=('EXTENSION_README.md' 'extension.cabal')

echo "String replacing files..."
for f in ${files[@]}; do
  if test -f $f; then
    sed "s/%PACKAGE%/$package/g" "$f" >replacement && mv replacement "$f"
    sed "s/%MODEL%/$model/g" "$f" >replacement && mv replacement "$f"
    sed "s/%LC_PACKAGE%/$lc_package/g" "$f" >replacement && mv replacement "$f"
    sed "s/%LC_MODEL%/$lc_model/g" "$f" >replacement && mv replacement "$f"
    sed "s/%LCC_MODEL%/$lcc_model/g" "$f" >replacement && mv replacement "$f"
    sed "s/%LCS_MODEL%/$lcs_model/g" "$f" >replacement && mv replacement "$f"
  fi
done

echo "Renaming files and directories..."
mv extension.cabal "${lc_combined}.cabal"
mv EXTENSION_README.md "README.md"
mv PACKAGE/MODEL.hs "PACKAGE/${model}.hs"
mv PACKAGE/MODEL/Handler/MODEL.hs "PACKAGE/MODEL/Handler/${model}.hs"
mv PACKAGE/MODEL "PACKAGE/${model}"
mv PACKAGE "${package}"

if [ `basename $(pwd)` = "scaffold-extension" ]; then
  echo "Renaming underlying directory..."
  cd ..
  mv scaffold-extension $lc_combined
  cd $lc_combined
else
  echo "Did not rename the underlying directory, as it is not named 'scaffold-extension'."
fi

echo "Removing left-over files..."
rm create_extension_scaffold.sh
rm LICENSE

echo "Done."
echo
echo "Please consult the documentation on how to install this extension in a base application."
echo "https://github.com/lambdacms/lambdacms/tree/master/scaffold-extension"
echo
