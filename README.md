HWebUI
======

Haskell User Interface Library

Early prototype.

Installation
============

Download sources with git, enter directory on command line prompt and type:

~~~
cabal install
~~~

This installs HWebUI with all its dependencies and also the
HWebUI-example1 executable.

If you don't want to change your Haskell environment, consider
replacing `cabal` by `cabal-dev` for a sandboxed install. See
[hackage/cabal-dev](http://hackage.haskell.org/package/cabal-dev).

Examples/Demo
=============

Run HWebUI-example-arithmetic, HWebUI-example-currency, HWebUI-example-counter, HWebUI-example-multiselect or HWebUI-example-crud and point your browser to:

http://localhost:8080/webgui

Wiki
====

See also the [Wiki](https://github.com/althainz/HWebUI/wiki) for
more information.

Haddock
=======
Build haddocks and read them. The haddock documentation has improved a lot and now contains usage examples, overview explanations and some insight into implementation details. Simply build with 

~~~
cabal haddock
~~~

and guide your browser to the result files in (http://dist/doc/html/HWebUI/frames.html).
