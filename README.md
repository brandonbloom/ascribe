# ascribe

A Clojure library providing Dynamic Reference Attribute Grammars.


## Status

During the development of [Fipp](github.com/brandonbloom/fipp), I stumbled upon
[Kiama](code.google.com/p/kiama/).  Fipp and Kiama provide a pretty printer
built on some of the same research.  Kiama, however, also provides libraries for
tree rewriting and attribute grammars.  After completing Fipp, I decided to study both of
those areas of computer science.  My project [Retree](github.com/brandonbloom/retree)
is a direct port of Kiama's rewrite combinators.  However, I decided to conduct
more research before tackling an implementation of attribute grammars.

Torbjörn Ekman's thesis stood out among the literature:
[Extensible Compiler Construction](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.100.6085).
Guided by this work, I build this prototype of Ascribe
[as it stands today](https://github.com/brandonbloom/ascribe/tree/6e13169d2077933e1a4f661e68880323c6a40705).

Then I stumbled upon [racr](https://code.google.com/p/racr/).  This has *a lot* in common
with my goals for Ascribe.  In light of the fact that there is new and exciting research
occuring in the Dynamic Attribute Grammar world, I'm planning to continue development of Ascribe
in the open.


## License

Copyright © 2013 Brandon Bloom

Distributed under the Eclipse Public License, the same as Clojure.
