#!/bin/sh

# declarations
output_dir="dist"
entry="src/Main.elm"
js_out="$output_dir/main.js"
debug=false

# setup build options
if [ $# -ne 0 ]; then
	debug=true
fi

if [ "$DEBUG" = true ] ; then
	debug=true
fi


# compile elm code
if [ "$debug" = true ] ;
then
	echo "Building for debug"
	elm make $entry --output=$js_out --debug
else
	echo "Building for prod"
	elm make $entry --output=$js_out
fi


# copy resources to dist
# markup and design
cp src/index.html $output_dir/index.html
cp src/index.css $output_dir/index.css


# assets
cp -rf assets/ $output_dir/assets/
