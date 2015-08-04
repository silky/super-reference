# super-reference

Web-based reference manager.

Point it at a bibtex file and it will list all the entries therein, allow you to search them and star them.

# Screenshot

![](screenshot.png)

# Installation

Clone

````
git clone https://github.com/silky/super-reference.git --recursive
cd super-reference
````

Sandbox and build
````
cabal sandbox add-source bibtexier
cabal install language-javascript
cabal install -j --enable-tests --max-backjumps=-1 --reorder-goals --force-reinstalls
cabal install yesod-bin
````

Run local dev server in bash (or whatever you use)
````
cabal exec bash
yesod devel
````
