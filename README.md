# gambit-build-framework
Author
======
- Frédéric Hamel

Summary
=======
This is a basic build framework to allow the build
of module using a simple syntax.

Compilation
===========
This library can be compile to a `.oN` by simply running
the compiler gsc-script on the file `\_build.scm`.

This can also be build using the `meson.build` script
and ninja like follow:
```
> meson . build
> ninja -C build
```

This will create both a `.c` and `.o1`.

Test
====
As a simple test there is a `_build-test.scm` that own the compilation rule to `test.scm`.
To run the test:
```
> gsc-script -i _build-test.scm
...
>
```
A directory `.builds` should be created with the build.

Installation
============
There is no installation process yet...
