buildDir=build/
srcDir=src/
if [ ! -d "$buildDir" ]; then
    mkdir $buildDir
else
    rm "$buildDir"*
fi

lineDirective=__LINE__
fileDirective=__FILE__
for fileName in "$srcDir"*; do
    buildDestination=$buildDir$(echo $fileName | sed "s|$srcDir||")
    sedCmd="s|$fileDirective|\"$fileName\"|"

    # replace each instance of $lineDirective with the current line in the file
    # then replace $fileDirective with the current file name
    awk "/$lineDirective/{gsub(/$lineDirective/,NR)}1" $fileName |
        sed $sedCmd >$buildDestination
done
