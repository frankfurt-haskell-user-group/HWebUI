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
HWebUI example executables.

If you don't want to change your Haskell environment, consider
replacing `cabal` by `cabal-dev` for a sandboxed install. See
[hackage/cabal-dev](http://hackage.haskell.org/package/cabal-dev).

Examples/Demo
=============

Run HWebUI-example-arithmetic, HWebUI-example-currency, HWebUI-example-counter, HWebUI-example-multiselect or HWebUI-example-crud and point your browser to:

http://localhost:8080/webgui

Further Information
===================

Haddock annotations are available in the source code, to build them run:

~~~
cabal haddock
~~~

alternatively the pre-build haddock docu is available here: http://althainz.github.io/HWebUI/HWebUI.html.
