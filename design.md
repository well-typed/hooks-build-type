Matthew Pickering, Sam Derbyshire, Duncan Coutts (Well-Typed LLP)

In this document we propose a design of a new `Hooks` build type,
which is intended to replace the `Custom` build type.

This work has been carried out by Well-Typed LLP thanks to investment from the Sovereign Tech Fund.


# Motivation

This work is intended to resolve one major architectural design flaw in Cabal.
Once resolved, this will establish the foundations for improvements in tooling
based on Cabal, and make Cabal easier to maintain for the long term.

A fundamental assumption of the existing Cabal architecture is that each package supplies its own build system, with Cabal
specifying the interface to that build system. Modern projects consist of many packages. However an aggregation of per-package
build systems makes it difficult or impossible to robustly implement cross-cutting features. This includes features in
high demand such as

* fine-grained intra-package and inter-package build parallelism;
* rich multi-package IDE support;
* multi-package REPL;
* doctests;
* cross-compilation.

The fundamental assumption has turned out to be false: all modern Haskell packages implement the build system interface
using a standard implementation provided by Cabal itself, albeit in some cases with minor customisations. Cabal already provides a build
system based on declarative configuration, and the majority of packages use this “simple build-type”. A minority use the
“custom build-type”, which allows wholesale replacement of the build system, but in practice is mostly used to make minor
customisations of the standard implementation. It is the existence of this custom build-type which is holding us back, but its
flexibility is mostly unused.

Thus the solution is to invert the design: instead of each package supplying its own build system, there should be a single build
system that supports many packages.

Of course, some packages still need to customise the behaviour of the build system such as running code during a
configuration step to gather information about the host system, where this dependency cannot be expressed declaratively.
Thus, for selected packages, replacing the custom build-type will require a new mechanism – a new “hooks build-type” – which
will permit controlled extensions to the build system. Moreover, the architecture needs to be flexible to accommodate future
changes, as new build system requirements are discovered.

By way of example, consider an IDE backend like HLS. An IDE wants to *be* the
build system (and compiler) itself, not for any final artefacts but for the
interactive analysis of all the source code involved. It wants to prepare (i.e.
configure) all of the packages and components in the project in advance of
building any of them, and wants to find all the source files and compiler
configuration needed to compile the components. This is incompatible with a set
of opaque per-package build systems, each of which is allowed to assume all
dependencies already exist, and which can only be commanded to build artefacts.
An IDE backend could however invoke per-package hooks at appropriate moments in
the preparation of a set of components in a project. For example it could
invoke configure hooks to embellish the component descriptions and could invoke
pre-build hooks which might generate particular modules. In summary, IDEs are
incompatible with each package supplying its own build system. IDEs need to
finely control a single build system which works across all packages.

The overall goal is the deprecation and eventual *removal of support for the
custom build-type*. It is the removal of the custom build type that will enable
simplifications and easier maintenance in Cabal, and enable easier
implementation of new features. The addition of a hooks build type is to
provide an alternative and a migration path for existing uses of the custom
build type that cannot be migrated to the simple build type.
This migration is intended to take place over a long timescale to allow
the gradual migration of packages which use the custom build type.

In the short term, the important parts of this project include:

 * Surveying existing Haskell packages that currently require the custom build-type, identifying where their requirements can be fulfilled with the simple build-type using existing declarative features, where new declarative features would be useful, or where extending the build system is necessarily required.

 * Engaging with package maintainers to remove the requirement for the custom build-type where better alternatives exist.

 * Designing a new hooks build-type for Cabal that will allow packages to customise the build system as needed, without replacing it wholesale. This design will involve collaboration with other stakeholders including tool developers such as the maintainers of GHC and the HLS.

 * An implementation of the proposed hooks build-type in the Cabal library.

The design and experimental implementation efforts inform each other, so it is expected that they proceed concurrently.

The remainder of document is a draft of the design for a new hooks build type.


## Issues with `UserHooks`

Redesigning the hooks interface gives us the opportunity to learn lessons from
the original `UserHooks` design.

The existing `UserHooks` mechanism used for `Custom` build type presents a few problems:

* It is too expressive, as it allows users to override entire build phases. This flexibility
  turns the building of a package into a blackbox, which pessimises certain parts
  of `cabal-install`.
* It often isn't possible to perform the customisation you need using only pre/post hooks, as they aren't expressive enough.
  This leads to users instead overriding the main phase, manually taking some pre/post steps
  and propagating the information to/from the main build phase.
* Customisation is not propagated. Any modifications to the project configuration have
  to be reapplied in each hook. This is quite unintuitive, as you would expect to only have
  to apply an update to e.g. the `LocalBuildInfo` once, instead of needing to re-apply it
  in every build phase.
* The `UserHooks` interface is not component aware. The hooks were designed in a pre-component era,
  so it's quite awkard to provide a hook which affects just one component. At best, it's possible
  to handle the main library and executables, but there is no concept of named sublibraries or
  of other component types such as testsuites or benchmarks in the `UserHooks` design.
* The API was hard to change in a backwards compatible way.

Relevant tickets for these issues include:

* [Tickets labeled `Hooks`](https://github.com/haskell/cabal/issues?q=is%3Aissue+is%3Aopen+Hooks+label%3A%22Cabal%3A+hooks%22)
* [Componentwise hooks](https://github.com/haskell/cabal/issues/7350)
* [Hook redesign](https://github.com/haskell/cabal/issues/3600)
* [LocalBuildInfo should change at build time when components are selected](https://github.com/haskell/cabal/issues/2910)

## A new design: `SetupHooks`

`Cabal` is augmented with a new build-type, `Hooks`.

The `Hooks` build-type represents a middle-ground of customisation which specifically
only permits *augmenting* the build process at specific points, while disallowing
the complete replacement of individual build phases.

To implement a package with `Hooks` build type, the user needs to provide
a `SetupHooks.hs` file which specifies the hooks.


## Goals of `SetupHooks`

The new `Hooks` build type should:

* subsume all existing usage of `Custom` setup scripts,
* provide a set of pre/post hooks which can be invoked at the relevant times to
  augment the build process of a package (without overriding any of the main `Cabal` build phases),
* be component-aware.
* Be independent, in the sense that each can be invoked separately by calling an executable to
  run each hook. This restriction prevents the hooks directly sharing state with each other as
  all the inputs and outputs must be serialised.
* Be easier to evolve over time. The `UserHooks` became ossified due to
  poor backwards compatibility.

## Non-goals of `SetupHooks`

The design of `SetupHooks` should be thought of as a successor to the `Custom` build type.
This means it inherits several aspects of the design of the `Custom` build type.
For example, `SetupHooks` will still require a separate `setup-depends` stanza
in the cabal file, and cross-compilation will still cause issues for `cabal-install`
just as with the `Custom` build type.

Although it is not possible or practical to address too many problems at once,
it is a goal to make the new hooks API more evolvable than the old `UserHooks`
API, and thereby it is hoped that it will be possible to adapt more easily to
future features and requirements (such as proper support for cross-compilation).

# High-level design of `SetupHooks`

* The user supplies a Haskell file named `SetupHooks.hs` which defines
  a value `setupHooks :: SetupHooks`, which is a record of user-specified
  hooks. Thus this interface is fundamentally a Haskell library interface, not
  a command line interface as with the classic `Setup.hs`.

* A hook is a Haskell function, with a type such as `HookInputs -> IO HookOutputs`
  appropriate for the phase.
  * `HookInputs` should include as much information as possible; this is the
    information from `Cabal` that we give the user access to.
  * Similarly, `HookOutputs` should be as large as possible, as this allows the user
    to influence more of the build process.
  * The above two points need to be balanced with ensuring a proper phase separation.
    See [§ Phase separation](#phase-separation).

* Each hook is optional, i.e. the user should provide a hook whose type has the
  form `Maybe (HookInputs -> IO HookOutputs)`.

  The explicit wrapping in `Maybe` means that `Cabal` will know when there
  is no hook at that particular stage.

* Hooks augment the fixed build process. They can run before or after certain
  phases of the build, but are not allowed to replace the "core" parts of the build
  pipeline.

* `SetupHooks.hs` can also be compiled to a separate executable, which can be called
  to invoke each hook in a standalone fashion.

  This will allow finer-grained build plans, as is described at the end of
  this document in [§ Future work](#Future-work).

* To support backwards compatibility with the existing `Setup.hs` CLI, Cabal is
  modified to provide a `defaultMainWithSetupHooks` function which will ensure
  that the `SetupHooks` are invoked in the correct place during the normal
  build pipeline.

## Phase separation

We want hooks to be "as powerful as possible", i.e. the input and output to any
given hook should be as large as possible, to allow users to use as much of the
information from `Cabal` as possible, and to influence as much as the build
process as possible.

The main consideration that constraints the space of possibilities is the need
for a proper phase separation:

* Only configure hooks can make changes to the `PackageDescription`.
  Once configuration is finished, the package description should be set in stone,
  and subsequent hooks such as build hooks should not be allowed to modify it.

  After the `PackageDescription` is finalised then `Cabal` computes additional
  information. Allowing modification of `PackageDescription` after this would
  lead to this additional information getting out of sync. Therefore there is a precise
  point where modification is allowed.
  See [§ Configuration hooks](#Configuration-hooks) for some more justification.

* Once the global package configuration is done, all hooks should work on a
  per-component level. This avoids introducing additional synchronisation points
  in a build that would limit the amount of available parallelism.


# Low-level design of `SetupHooks`

The primary part of the design process is to work out the location and interface
for the hooks.

We want to arrive at a design by two means:

* The consideration of the existing usage of `Setup.hs` scripts to guide
  what hooks should be able to do.
* A high-level understanding of what the build process of a package should
  look like, taking into account concerns such as parallelisability.

These two viewpoints can inform each other about the precise details for the design.

## Configuration hooks

The most important hook types are the configure hooks.

A principle that we want to abide by is that configuration time is when are
taken all the decisions about how to build the subsequent stages of a project.

Therefore, it is the hooks to the configuration phases which have the ability to augment
the build environment with additional settings. The hooks for other phases must honour
this configuration.

For example, a `preBuild` hook should do some **building** of its own rather than
calculating any options for the build phase - that should be done during configuring.


### Cabal configuration type hierarchy

Cabal has a number of datatypes which are used to store the result of
configuration. We will briefly describe them here before getting into
the precise design of the hooks.

* `LocalBuildInfo` - This stores the whole result of configuring
* `GenericPackageDescription` - The parsed version of a .cabal file
* `PackageDescription` -  A resolved `GenericPackageDescription`, flattened relative to a flag assignment.
* `Component` - A sum-type which captures the possible different component types (these live in a `PackageDescription`)
  * `Library`   - A library component (stored inside a `Component`)
  * `Executable`, ... - other kinds of components
* `BuildInfo` - The shared part of a component which describes the options which will
                be used to build it.
* `ComponentLocalBuildInfo` - Additional information which `Cabal` knows about a component which is not present in `Component`.
   This is usually pieced together from the parent `LocalBuildInfo` and the individual `BuildInfo` of the `Component`.
* `ConfigFlags`/`BuildFlags`/`HaddockFlags` - Flags to `./Setup configure`, `./Setup build`, `./Setup haddock` etc.

### Global configuration vs Component configuration

It is also helpful to keep in mind the difference between global and local configuration.

There are parts of the `LocalBuildInfo` which are decided upon at a global level.
For instance, whether want to build dynamic libraries.
On the other hand, there are also things we want to decide on a per-component level,
such as specific GHC options to compile a component with.
In designing a hooks interface, it is be desirable to separate these two concerns.

Moreover, there are parts of the `LocalBuildInfo` which we are not allowed to modify.
For example, things such as package dependencies can't be modified because they are
used externally by `cabal`'s solver to construct the overall build plan. It is
desirable to design a `Hooks` interface which prevents the modification of these
parts of `LocalBuildInfo`.

We propose to achieve this by splitting up the `LocalBuildInfo` datatype into
two components, `LocalBuildDescription` and `LocalBuildConfig`, with the former
containing the information that shouldn't be available for modification by
the hooks.

### Design of configuration hooks

We propose to add the following configuration hooks:

```haskell
type PreConfPackageHook =
  ConfigFlags -> LocalBuildConfig -> Compiler -> Platform -> IO LocalBuildConfig
type PostConfPackageHook =
  LocalBuildConfig -> PackageBuildDescr -> IO ()
type PreConfComponentHook =
  LocalBuildConfig -> PackageBuildDescr -> Component -> IO ComponentDiff
type PostConfComponentHook =
  LocalBuildInfo -> Component -> IO ()

data ConfigureHooks
  = ConfigureHooks
  { preConfPackageHook    :: Maybe PreConfPackageHook
  , postConfPackageHook   :: Maybe PostConfPackageHook
  , preConfComponentHook  :: Maybe PreConfComponentHook
  , postConfComponentHook :: Maybe PostConfComponentHook
  }
```

The configuration phase goes as follows:

- Firstly, decide on the initial global configuration for a package,
  and store the results in `LocalBuildConfig`.

- Then run the `preConfPackageHook`, which has the opportunity to modify the
  initially decided global configuration (stored in `LocalBuildConfig`). After
  this point, the `LocalBuildConfig` can no longer be modified.

- Then use the `LocalBuildConfig` in order to perform the global package
  configuration.

- Afterwards, allow the user to inspect the result of the global configuration
  in the `postConfPackageHook`.

- Then we configure on a per-component basis.

    - Before configuring a component, run the `preConfComponentHook`, this is the only means
      to apply specific options to a `Component`.

    - Then run the per-component configuration, this performs some calculations to
      create the `ComponentLocalBuildInfo` datastructure.

    - Then run the `postConfComponentHook` to allow inspection of the per-component
      configuration.

#### Using configuration hooks

The configuration hooks follow a simple philosophy:

> If you want to modify a global package option, then you have to use the `preConfPackageHook`.
  If you want to modify a component configuration option, you have to use `preConfComponentHook`.

If you modify the options in these phases then the configuration is propagated
into all subsequent phases, and the design of the interface ensures that
this is the only point where you can modify the options.

It is only the pre-configuration hooks which allow modification of the options.
This is because the configuration process computes some more complicated data
structures from these initial inputs. If you were allowed to modify the results
of configuration then it would be error-prone to ensure that you suitably updated
both the options in question as well as the generated configuration.
For example, both `PackageDescription` and `ComponentLocalBuildInfo` contain
a list of exposed modules for the library.
This is why the "post" configuration hooks can only run an `IO` action;
they can't return any modifications that would affect the `PackageDescription`.

#### `ComponentDiff`

The `ComponentDiff` records the modifications that should be applied to each component.

For each component, `preConfComponentHook` is run, returning a `ComponentDiff`.
This `ComponentDiff` is applied to its corresponding `Component`
by monoidally combining together the fields.

```haskell
newtype ComponentDiff = ComponentDiff { componentDiff :: Component }

emptyComponentDiff :: ComponentName -> ComponentDiff
```

The diff is represented by a `Component`; not all fields of a `Component` are allowed
to be modified, and when the diff is applied it is dynamically checked that we do not
modify anything which we shouldn't.

An alternative design would be to define a custom diff datatype which statically
distinguishes which fields are allowed to be modified. This was deemed more difficult
to maintain, as it requires `Cabal` developers to keep the `Component` and `ComponentDiff`
types up-to-date.

## Configuration hooks examples

The `Configure` build type can be implemented using these new configuration hooks.

- The ./configure script is executed once in a `preConfPackageHook` and produces
  the `<pkg>.buildinfo` file which contains the modified `BuildInfo` for main library
  and executable component.
- In the `preConfComponentHook` the `<pkg>.buildinfo` field is read from disk and
  the configuration is applied to each component.

The new design allows the `./configure` script to only be executed once per package and
the result shared across the configuration of components.

## Build hooks

The build hooks are intended to perform additional steps before or after
the normal build phase for a component. These steps cannot change
the configuration of the package; they can only perform side-effects.
The intention is that you should separate any cheap configuration
from expensive building.

One crucial observation that factors into the design of build hooks is that
many different `cabal` phases can be thought of as building something.
Indeed, preparing an interactive session or generating documentation
share many commonalities with a normal build.
For example, if one is generating modules in a pre-build phase, one
should also generate these same modules before running `GHCi` or `haddock`.

For this reason, we propose abstracting over these different build-like phases
in the build hooks:


```haskell
data BuildingWhat
  = BuildNormal   BuildFlags
  | BuildRepl     ReplFlags
  | BuildHaddock  HaddockFlags
  | BuildHscolour HscolourFlags

type BuildComponentHook =
  BuildingWhat -> LocalBuildInfo -> TargetInfo -> IO ()

data BuildHooks
  = BuildHooks
  { preBuildComponentHook  :: Maybe BuildComponentHook
  , postBuildComponentHook :: Maybe BuildComponentHook
  }
```

This design ensures that the build hooks are consistently run in all
build-like phases. This reflects the common pattern in custom Setup scripts
that one would update the `haddock` and `repl` hooks to mirror the `build` hooks
(which one can easily forget to do, and end up with an unusable `repl`, for example, see `singletons-base`
which fails to update the `replHook`).

- The `preBuildComponentHook` is run just before each component is built.
  The hook has access to the specific information to each component.

  This hook can, for example, be used to generate modules in the component.

- The `postBuildComponentHook` is run after each component is built.

  It can be used to do further work after building. For example, in `Agda`,
  once the `agda` executable is built we might want to run it to compile
  the builtin libraries and generate interface files for them.


In the future, it would be good to investigate allowing these hooks to return
dependency information to support Cabal's recompilation checker in deciding whether
it needs to rerun the hooks or not. This is particularly relevant when building documentation,
as one doesn't want to have to run the hooks twice in the workflow `cabal build && cabal haddock`.

* An example bug caused by having to duplicate the "build" hook in all phases - https://github.com/haskell/cabal/issues/9401

## Copy hooks

The `copy` hooks run before and after the copy phase, which moves build artifacts
from the build directory into the install directory.

```haskell
type CopyComponentHook =
  LocalBuildInfo -> CopyFlags -> TargetInfo -> IO ()

data CopyHooks
  = CopyHooks
  { preCopyComponentHook  :: Maybe CopyComponentHook
  , postCopyComponentHook :: Maybe CopyComponentHook
  }
```

The copy hooks can be used to copy files per-component. For example, in the Agda
example mentioned for `postBuildComponentHook`, it can be used to copy over the
generated `.agdai` interface files when installing the compiler.

It is important that these copy hooks are also run when installing, as this
fixes the inconsistency noted in [Cabal issue #709](https://github.com/haskell/cabal/issues/709).

# `SetupHooks` in practice

These examples are based on the corresponding survey we undertook of packages
using custom `./Setup.hs` scripts.

### Generating modules

One of the main uses of `Setup.hs` scripts is to generate modules.

To achieve this with `SetupHooks`, in the per-component configure hook
`preConfComponentHook`, declare that you **will** generate certain modules.

These modules can be generated either in this configure hook itself,
or in the (per-component) pre-build hook. It depends on what kinds of modules
you are generating and how you are generating them as to which method is suitable.
One reason to choose to generate in pre-build might be that there is the entire
configuration information available at that point.

### ./configure style checks

Checks for things to do with the system configuration can either be
performed in the global configure step or in a per-component configure step. The
results of these checks are then propagated into subsequent phases.

In addition, the `Configure` build-type can be implemented in terms of `SetupHooks`
by running the script in the global configuration step, and then applying the
result in the per-component configure hook.
This also allows defining a variant of the `Configure` build type, using
`SetupHooks`, which is component-aware.

## Backwards compatibility

In order to maintain backwards compatibility with build systems which solely use
the `./Setup.hs` interface (such as `nixpkgs` and other distributions), when
a user users the `Hooks` build type, we automatically generate a shim `Setup.hs`
of the following form:

```haskell
import Distribution.Simple (  defaultMainWithSetupHooks )
import SetupHooks ( setupHooks )

main = defaultMainWithSetupHooks setupHooks
```

This allows `Hooks` projects to continue to work as normal when built for
distribution by a package maintainer.

# Testing and migration

We have created the [`hooks-setup-testing`](https://gitlab.haskell.org/mpickering/hooks-setup-testing/)
repository. This repository consists of patched packages, migrated to use `build-type: Hooks` instead of
`build-type: Custom`.

This repository follows the same design as `head.hackage`, as it provides an overlay package repository,
but instead of enabling building against GHC HEAD it instead provides our patched version of `cabal-install`
and builds packages using our patched `Cabal` libraries.

This repository fulfills several goals:

* Design
  - By collecting all the patches in one place, we can be sure that the design of the hooks is
    robust enough to handle the common use-cases.
* Testing
  - The CI in the repository ensures that our patches continue to build against
    migrated packages, as the design of the `Hooks` interface evolves.
* Overlay
  - Users can use the repository as an overlay like `head.hackage`, allowing them to
    experiment with the new `Hooks` design locally.
  - The overlay will also allow us to test new features of `cabal-install` which rely on
    having migrated packages away from `build-type: Custom`.
* Migration
  - The patched packages can be used by library authors to migrate their own packages
    when the time comes.
# Future work

In the future, it will be desirable for a tool like `cabal-install`
to avoid using the `./Setup` CLI interface at all.

* There is no need to perform configuration because `cabal-install` has already
  decided about the global and local configuration.
* It will allow finer grained build plans, because we don't have to rely
  on `./Setup.hs build` in order to build a package.
  `cabal-install` could build each module one at a time.
* It will allow `cabal-install` and other tools to use the `Cabal` library
  interface directly to manage the build process, which is richer and more
  flexible interface than what can be provided by a CLI.
* `cabal-install` will be able to use per-component builds for all packages,
  where currently it must fall back to per-package builds for packages
  using `build-type: Custom`. This will reduce the number of different code
  paths and simplify maintenance.
