#!/bin/env bash
# WSL - Launch html file into a browser

# Will convert a file path from wsl to windows format, prepends file://, and launches it with explorer.exe. This will cause the default browser to open the file. 


# [[file:../README.org::*WSL - Launch html file into a browser][WSL - Launch html file into a browser:1]]
#!/bin/env bash

filepath="$1";
[ "" == "$1" ] && filepath=".";
if [ ! -e "${filepath}" ]; then
    echo "  Did not find the file, \"$filepath\". Unable to continue.";
    return;
fi;
fullfilepath="$(realpath "${filepath}")"
winfilepath="$(wslpath -m "$fullfilepath")"
echo "  Opening \"${fullfilepath}\" using explorer.exe.";
echo "  Opening \"${winfilepath}\""
explorer.exe "file://${winfilepath}"
# WSL - Launch html file into a browser:1 ends here
