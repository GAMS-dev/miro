
#!/bin/bash

# signs executables and libraries inside a directory recursively
# Provide (absolute) directory path as first argument
# codesign identity as second argument
# entitlements file as third argument

if [ -z "$1" ]; then
    echo "Please provide directory path as first argument"
    exit 1
fi
if [ ! -d "$1"  ]; then
    echo "Directory $1 does not exist"
    exit 1
fi
if [ -z "$2" ]; then
    echo "Please provide codesign identity as second argument"
    exit 1
fi
if [ -z "$3" ]; then
    echo "Please provide entitlements file as third argument"
    exit 1
fi
if [ ! -f "$3"  ]; then
    echo "Entitlements file $3 does not exist"
    exit 1
fi

find "$1/Frameworks" -type f \( -name "*.so" -o -name "*.dylib" \) -exec sh -c "file '{}' \; | grep -q -e 'library x86_64\|bundle x86_64'" \; -print0 | \
   xargs -0 codesign --sign "$2" --force -v --timestamp || exit 1

find "$1/Resources" -type f \( -name "*.so" -o -name "*.dylib" \) -exec sh -c "file '{}' \; | grep -q -e 'library x86_64\|bundle x86_64'" \; -print0 | \
   xargs -0 codesign --sign "$2" --force -v --timestamp || exit 1

find "$1/Frameworks" -type f -perm +0100 -exec sh -c "file '{}' \; | grep -q 'Mach-O 64-bit executable'" \; -print0 | \
   xargs -0 codesign --sign "$2" --force -v --options runtime --timestamp \
        --entitlements "$3" || exit 1

find "$1/Resources" -type f -perm +0100 -exec sh -c "file '{}' \; | grep -q 'Mach-O 64-bit executable'" \; -print0 | \
   xargs -0 codesign --sign "$2" --force -v --options runtime --timestamp \
        --entitlements "$3" || exit 1

find "$1/Frameworks" -type d -name "*.framework" -print0 | \
   xargs -0 codesign --sign "$2" --force --timestamp || exit 1

codesign --sign "$2" --deep --force --options runtime -v --timestamp --entitlements "$3" "$1/../../GAMS MIRO.app" || exit 1

exit 0
