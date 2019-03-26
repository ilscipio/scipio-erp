SCIPIO: This folder contains general libraries needed by ant scripting tasks 
but that do not need to be on the scipio runtime classpath nor part of the scipio main build.
Essentially these could have their own dedicated ivy.xml (could in future).

These may be downloaded on-demand, but currently for compatibility are mostly part 
of "base" conf as well (with relocations done in base/build.xml) because many ant tasks 
simply assume "build" takes care of everything.

