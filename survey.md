# Stackage Setup.hs survey

A primary goal of the `Hooks` build-type is to replace the `Custom` build-type.
In order to inform the design of the `Hooks` build-type, we want to analyse
what people use the `Custom` build-type for.

# Methodology

For our survey, we took the latest stackage LTS snapshot (`LTS-21.9`), downloaded
all the packages and searched for the ones which contained `Custom` `Setup.hs` scripts.

`LTS-21.9` contains 3010 packages; we searched these packages using the following
regular expression in order to identify the packages with `build-type: Custom`.

```
$ grep -Einr "\bCustom\b" --include \*.cabal ./
```

This resulted in a candidate list of 87 packages (3.2%) which we then manually
investigated to determine whether:

* The package did indeed use a custom `Setup.hs` script; we recorded the reason why
  and summarised the results in this document.
* The result was a false-positive (and hence discarded from the survey). For example, some
  packages contained cabal files for testing purposes. 20 results were discarded in this way.


## Blind-spots

* Our survey concentrated on packages which are in Stackage. This only represents
  a small and maintained set of packages. It seemed important to start the survey with
  a manageable set of packages but also packages which were being actively maintained.
  The design of the `Hooks` mechanism should be informed by how people are using the
  `Custom` build-type in conjunction with modern `Cabal` declarative features.
* There are probably a number of private packages used by commercial companies which
  we couldn't consider for our survey. When we solicit feedback from the community
  about the hooks mechanism we hope that any consumers of the `Custom` build-type will
  provide their perspective about whether the `Hooks` build-type will subsume their use case.

# Survey results

## Generating modules

The most common usage of a `Setup.hs` script is to generate modules in a project.
The following sections give some example use-cases.

### Doctests

Packages that rely on the `cabal-doctests` package for their doctests usually define
a `Custom` setup script in terms of `defaultMainWithDoctests`. This generates the module
`Build_doctests`, which can be imported by a testsuite in order to start a `doctest`
session which uses the exact same GHC options to build the doctests as was used to compile
the original package.
It is intended that the testsuite will then define a test which imports this module,
and then invokes doctests with the options which were saved in the file. It seems that
this approach works fine in practice but really there is no guarantee that at
the runtime of the test executable that the same environment will exist.

Packages:

```
aeson-diff-1.1.0.13, attoparsec-time-1.0.3, focuslist-0.1.1.0, haskell-gi-0.26.7, kleene-0.1, openapi3-3.2.3, password-3.0.2.1, password-instances-3.0.0.0, password-types-1.0.0.0, pcg-random-0.1.4.0, polysemy-1.9.1.0, polysemy-plugin-0.4.5.0, pretty-simple-4.1.2.0, servant-auth-docs-0.2.10.0, servant-openapi3-2.0.1.6, servant-swagger-1.1.11, swagger2-2.8.7, twitter-conduit-0.6.1, wai-logger-2.4.0, wreq-0.5.4.2, xml-conduit-1.9.1.3
```

The ideal design for doctests would be if cabal had specific support for
doctests.

```
test-suite doctest
  type: doctest
  doctest-for: <library-name>
```

Then `cabal` could arrange for the `doctest` executable to be called with the
same arguments which were used to build the `library` component. This aligns with the
point of view that "running doctests" is a different means of intepreting a
component whereas the normal means is to compile the source files to object files.
Perhaps this point of view points to the existence of a `cabal doctest` command, as
`cabal repl`, and `cabal haddock` already provide different "views" of the same component.

The `singletons-base` package defines similar logic for its own testsuite. The testsuite takes
the path to the compiler and the GHC options used to build the package before invoking
GHC to compile modules in the testsuite. This is slightly more complicated than the
doctest example because the compiler is invoked to compile multiple files during
the running of the testsuite.

### haskell-gi packages

Another family of module generation tasks is to read a specification file and generate
modules based on that. The `haskell-gi` package does this for a number of GUI
libraries.

```
gi-atk-2.0.27, gi-cairo-1.0.29, gi-dbusmenu-0.4.13, gi-dbusmenugtk3-0.4.14, gi-freetype2-2.0.4, gi-gdk-3.0.28, gi-gdkpixbuf-2.0.31, gi-gdkx11-3.0.15, gi-gio-2.0.32, gi-glib-2.0.29, gi-gmodule-2.0.5, gi-gobject-2.0.30, gi-graphene-1.0.7, gi-gtk-3.0.41, gi-harfbuzz-0.0.9, gi-javascriptcore-4.0.27, gi-pango-1.0.29, gi-soup-2.4.28, gi-vte-2.91.31, gi-webkit2-4.0.30, gi-xlib-2.0.13
```

This is simple to support because the generation scripts do not depend on
any other results of configuration. In contrast to the doctest module generation,
the names of the generated modules is not known beforehand. This doesn't add any
additional complications but the additional dynamism is worth keeping in mind.

### Other module generation

There are a few other packages which generate custom modules.

```
ghc-paths-0.1.0.12, configuration-tools-0.6.1, nix-paths-1.0.1, stack-2.9.3.1
```

`ghc-paths` is a library which records the build-time location of the ghc binary
and it's libdir. It can be convenient to use `ghc-paths` when writing GHC API applications
because it saves you having to provide the correct libdir. This is at the cost of
making your application unsuitable for distribution (as the locations during build-time will
almost certainly not exist on your users system at runtime).  
This is achieved using a custom setup which defines some CPP macros which are
used to partially generate a module.

`nix-paths` is similar to ghc-paths but for nix related tools rather than ghc tools.
The `Setup.hs` script adds several nix utility programs to `hookedPrograms` before
their paths are propagated into the `Nix/Paths.hs` module by defining CPP macros.

`stack` contains a Setup.hs which persists information about the dependencies of
a library so that the dependencies that the executable is built against can be displayed
to the user via a command line flag. This usage seems fine because it does not rely
on these dependencies existing at stack executable runtime.

`configuration_tools` generates something similar to Cabal's built-in PackageInfo module
but with more information.

These examples are similar to the `Paths_*` and `PackageInfo_*` module which can be automatically
generated by Cabal in order to reflect information about a package. https://cabal.readthedocs.io/en/3.10/cabal-package.html#accessing-data-files-from-package-code

### Existing module-generation functionality

There are several declarative methods built into Cabal for generating modules.

#### Program generators baked into Cabal

https://cabal.readthedocs.io/en/stable/cabal-package.html#modules-and-preprocessors

Support for serveral common program generators is built into Cabal.
Cabal understands that files with a specific suffix are used
for module generation. It will then call the program generator itself on these
files in order to generate the modules.

* .gc (greencard)
* .chs (c2hs)
* .hsc (hsc2hs)
* .y and .ly (happy)
* .x (alex)
* .cpphs (cpphs)

This captures common and simple program generation tasks where one file
corresponds to one module to generate.

This list of modules can be extended by augmenting `hookedPreProcessors`
field of `UserHooks`.

#### Code-generators declarative feature

The `code-generators` feature was intended to provide a more customisable way to
use module generation. In particular it was intended to provide a declarative way to
implement support for the doctests test style.

The user specifies as Haskell executable target in the `code-generators` field.

```
test-suite doctest
   import: libdeps
   type: exitcode-stdio-1.0
   build-tool-depends: doctest-codegen-driver:doctest-codegen-driver
   main-is: Main.hs
   hs-source-dirs: test
   build-depends: cabal-doctest-demo, doctest
   code-generators: doctest-codegen-driver
```

The code generator is then executed and passed as arguments

* The source directories of the local components which are depended upon.
* The options which will be used to compile the test-suite executable.

and it returns via stdout:

* A list of modules which have been generated.

This has been used to implement a different approach to running doctests (https://github.com/gbaz/cabal-doctest-demo/)

There are a couple of issues with the current design of the feature:

* `code-generators` is only supported in the testsuite stanza. Most program generation
   tasks are for the library stanza so this limits the utility.
* For the `doctest` use-case, the generation script is passed a list of source directories
  for which it then has to traverse to find `.hs` files (which may or may not be part of the project).
* The doctests are compiled with the options used to compile the test-suite, which may be the
  correct thing to do, but I think from the perspective of a doctest writer that you would
  expect a doctest (written in the library component) to be compiled with the library
  options. At least the documentation should be quite clear about this (https://github.com/haskell/cabal/issues/9238).

As such, for more people to use this feature it seems a little more work in smoothing
off these edges would be beneficial.

#### Paths_* PackageInfo_* modules

A basic amount of introspection can be performed with `Paths_*` and `PackageInfo_*` modules.
It is possible that these modules could be extended to capture more information about
a package, and then custom Setup.hs scripts could be removed (if they just needed
the available information).

## Configure-style checks

Some packages implement configure-style checks in the Setup.hs script.

```
entropy-0.4.1.10, termonad-4.5.0.0, hlibsass-0.1.10.1, HsOpenSSL-0.11.7.6, mysql-0.2.1, postgresql-libpq-0.9.5.0
```

For normal library dependenices there is already the `pkg-config-depends` field
which allows dependencies on system libraries to be declared. `postgresql-libpq` allows
a user to optionally use this feature rather than relying on `pkg-config`.

The `hlibsass` library additionally compiles a C library during the build process.
This could be performed during a pre-build step.

The `Configure` build type is also implemented in terms of overriding the `confHook`
phase. We should also think about how to migrate these style of packages to our new hooks design.

## Augmenting hookedPrograms

Some packages are augmenting `hookedPrograms`, the reason for this is so that
you can pass the responsibility of finding the executables to `Cabal` and then
use the existing infrastructure for calling programs at a later point in your `Setup.hs`
script.

## Agda

```
Agda-2.6.3
```

The Agda compiler is a standard Haskell executable. It can be built and installed with
Cabal. Once the compiler is built, the base libraries need to be built by the Agda compiler so
that people can compile Agda files which depend on them.

The Hooks mechanism is used to modify the install phase so that during the install
process the Agda compiler will compile the base libraries. The installation is also
augmented so that the interface files for these libraries are installed into a place
where the compiler will subsequently find them.

## Idris

```
idris-1.3.4
```

The original Idris compiler uses a custom `Setup.hs` for similar purposes to Agda.

* Some modules are generated to record information about the build configuration into
  the library.
* When installed, additional libraries are built and copied.
* When cleaning, the additional libraries are removed.

We have previously considered these use-cases and it seems that migrating this Setup.hs
can be achieved by similar means to what we have already described.


## `executable-hash` package

```
executable-hash-0.2.0.4
```

After the test executable has been built, it injects a hash into the executable.
This should probably be in a per-component postBuild step. The test executable
inspects its own hash when run.

## `include-file` package

```
include-file-0.1.0.4
```

A file is generated which can be used in the testsuite for testing including the
file. It's crucial the file is generated before the testsuite is built, because it is
included into the test executable at compile-time via template haskell.

Not sure this is necessary, the test file could just be checked into the repo.

## Packages defining `__HADDOCK__` macro

There are a couple of packages which modify the invocation of the `haddock` program
so that a `__HADDOCK__` macro is defined when building documentation for the package.

```
system-filepath-0.4.14, threads-0.5.1.7
```

Since 1.20.0.0 Cabal has defined the `__HADDOCK_VERSION__` macro for this
purpose so we have prepared patches to remove the custom Setup.hs for this use-case.

## Packages performing workarounds for Haddock

```
Rattus-0.5.1
```

The `Rattus` package doesn't work with Haddock so additional options are passed
to get it to work. This seems to be a bug somewhere in Haddock, so it would be worthwhile
to investigate the reason for this failure.

In any case, it does point to the need to be able to specify different options to
`haddock`, `repl` and other commands. A suitable way to do this might be to allow
the user to modify the `BuildOptions`/`ReplOptions` etc, which support passing additional
arguments by using flags such as
`--repl-option` and `--haddock-option`.

## Packages overriding hooks

```
interpolatedstring-perl6-1.0.2
```

`interpolatedstring-perl6` overrides the whole testHook phase in a crude manner.
This is undesirable as it doesn't respect the configured compiler or other
configuration options.
The testsuite can be modified to fit into the standard test style.

