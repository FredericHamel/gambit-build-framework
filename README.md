# gambit-build-framework
Author
======
- Frédéric Hamel

Summary
=======
This is a basic build framework to allow the build
of module using a simple syntax.


Syntax
======
Here is a possible example of a ***package.sc*** which
specify how to build the project.

```
;; Specify a project with a single file "main.scm" linked against X11
(build#setup)
(build#link
  (build#make-project
    (build#add-source preload: #t "main.scm"))
  link-with: "X11")
```

Compilation
===========
This library is simply compiled with the ``build.sh`` script.
The generated executable is placed under the bin folder.

```
> ./build.sh
```

The execution of ***package.sc*** is done by using the generated program (./bin/\_build).
This will rebuild this program by using itself,
the generated executable is located under the folder ***$PWD/.gambit_v4.8.9@C***.
```
% ./bin/_build -exe
/a/b/c/.gambit\_v4.8.9@C/_build
%
```

Installation
============
There is no installation process yet...
