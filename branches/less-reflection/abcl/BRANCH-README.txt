The purpose of this branch is to implement a new mechanism to compile functions
to Java classes and to load those functions from fasl files.

Now each function is always loaded by name and instantiated by reflection.
However it should be possible to avoid reflection in many cases, for example
when loading local functions. In general the objective is to reduce the use of
reflection as much as possible: we are considering the option of emitting a
master loader class/function per FASL which will instantiate all the functions
defined in the FASL using "new".

To fully reach the objective we need:

1. a classloader that knows how to load stuff from fasls
2. a way of generating a loader class/function per fasl
3. replacing all calls to loadCompiledFunction and similar with "new"

status as of 2010-04-23:

1. this is the value of *fasl-loader*, bound per-fasl, defined in FaslClassLoader.java

2. todo

3. done for local functions only
