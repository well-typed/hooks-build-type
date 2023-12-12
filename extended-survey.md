# Extended Survey

Rodrigo Mesquita (Well-Typed LLP)

The [original survey](./survey.md) looked at uses of `Custom` in the Stackage
package set, and we subsequently [experimentally migrated these packages to use
`Hooks`](./hooks-uses.md).

In subsequent discussion of the design it became apparent that it would be
useful to consider a wider range of Haskell packages which might exercise corner
cases, so we expanded the survey to look at popular packages in the Hackage
package repository that use `Custom`.

This work has been carried out by Well-Typed LLP thanks to investment from the Sovereign Tech Fund.


## Methodology

For the extended survey, we manually reviewed the `Setup.hs` of the top 100
packages on Hackage (sorted by Downloads) that use a `Custom` build-type.  For
each package, we made very brief notes on the use of `UserHooks` in `Setup.hs`.

The [script to fetch these packages](extended_survey/Main.hs) and the [results
of running the script](extended_survey/top-hackage.txt) are available in this
repository.

In the results:

 * "done" means we have previously accounted for the package (see [`hooks-uses.md`](./hooks-uses.md)
   and the [`hooks-setup-testing` repo](https://gitlab.haskell.org/mpickering/hooks-setup-testing))

 * "doctests" means the package uses `Custom` only to implement doctests via `defaultMainWithDoctests`


## `hookedPreProcessors` and `ppOrdering`

There are many packages which make use of the `hookedPreProcessors` hook, which
allows authors to pre-process using the defined pre-processors various files
into Haskell modules.

HookedPreProcessors are formed of a suffix of the files that can be preprocessed
by this hooked preprocessor, and an action that produces a `PreProcessor`
comprised of an action to transform the file with the associated suffix into a
Haskell module, and a `ppOrdering` function to re-order the sequence in which
modules are preprocessed.

There are a few hookedPreProcessors using custom tools for pre-processing files
into Haskell modules, and a big use-case of hookedPreProcessors is C2HS
preprocessing.

`ppOrdering` is a bit of a weird function, but some preprocessors need to
re-order modules to guarantee the preprocessing works correctly. A prominent
case is C2HS:

* C2HS modules can import other C2HS modules.
* When a C2HS module is processed, it generates an interface file which is used
    by the modules importing it.
* If a C2HS module imports a C2HS module that has not yet been preprocessed,
    the interface file is unavailable and therefore preprocessing will fail.

`ppOrdering` allows arbitrary re-ordering to fix this class of issues.


## Results

Agda:
    done

pretty-simple:
    doctests

xml-conduit:
    doctests

entropy:
    done

HsOpenSSL:
    done?

wai-logger:
    doctests

postgresql-libpq:
    done

git-annex:
    implements build-type: Configure

libarchive:
  codegen c2hs?
  ```haskell
  import Distribution.C2Hs (defaultMainC2Hs)
  main = defaultMainC2Hs
  ```

glib:
  ```haskell
  -- Adjustments specific to this package,
  -- all Gtk2Hs-specific boilerplate is kept in
  import Gtk2HsSetup ( gtk2hsUserHooks ) -- gtk2hs-buildtools
  ```

pango:
  uses gtk2hsUserHooks
  writePangoVersionHeaderFile (reads from pkgconfig etc...)
  ```haskell
      [ "#define PANGO_VERSION_MAJOR " ++ show major
      , "#define PANGO_VERSION_MINOR " ++ show minor
      , "#define PANGO_VERSION_MICRO " ++ show micro
      ]
    where
      targetDir  = autogenPackageModulesDir lbi
      targetFile = targetDir </> "hspangoversion.h"
  ```

wreq:
    doctests

ghc-paths:
    done

cairo:
    gtk2hsUserHooks

polysemy:
    doctests

system-filepath:
    haddock hook to pass it ["--optghc=-D__HADDOCK__"]

ghc:
    postConf hook that generates multiple files (e.g. GHC.Settings.Config),
    converts primops to cli arguments
    only uses `LocalBuildInfo -> IO ()` in `ghcAutoGen`

swagger2:
    doctests

gi-javascriptcore (and other gis that I didn't add above)
    setupBinding from GIs

darcs:
    done

HTF:
  ```haskell
  preTestHook args flags =
      do system ("bash ./scripts/prepare")
         preConf simpleUserHooks args flags
  ```

openapi3:
    doctests

servant-openapi3:
    doctests

servant-swagger:
    doctests

gtk:
    gtk2hsUserHooks

ghc-prim:
  ```haskell
  regHook = addPrimModule
          $ regHook simpleUserHooks,
  buildHook = build_primitive_sources
            $ buildHook simpleUserHooks,
  haddockHook = addPrimModuleForHaddock
              $ build_primitive_sources
              $ haddockHook simpleUserHooks }
  ```

singletons-base:
    done

shh:
    doctests

influxdb:
    doctests

SDL:
    autoconfUserHooks

idris:
    done 90%

madlang:
  Does a lot of actions *before* `defaultMain`.
  They could be done in pre conf
  ```haskell
  main = sequenceA_
  [ setManpathBash
  , setManpathZsh
  , setManpathFish
  , writeManpages "man/madlang.1" "madlang.1"
  , writeBashCompletions "madlang"
  , defaultMain
  ]
  ```

pandoc-citeproc:
    installs man page with post copy hook

polysemy-plugin:
    doctests

mysql:
    done

hashed-storage:
  test hook runs `system some_script`
  buildHook and haddockHook:
  ```haskell
  -- Add custom -DFOO[=BAR] flags to the cpp (for .hs) and cc (for .c)
  -- invocations, doing a dance to make the base hook aware of them.
  ```
  They update package description with new build info
  and then update localPkgDescr in LocalBuildInfo.
  This is likely a trivial ComponentDiff in SetupHooks

ghc-heap-view:
 * post install hook to load into `ghci` a script from the installation
   directory and piping the output to ~/.ghci
 * post conf hook to give an error when the library is built with profiling

pcg-random:
    doctests

accelerate:
    doctests

password:
    doctests

interpolatedstring-perl6:
    done:

ghc-mod:
    * install, copy: they have in common that they invoke xInstallTarget which does something not trivial
    * build: patchLibexecdir of LBI
    * hookedprograms: simpleProgram shelltest

liquidhaskell:
    * Calls `liquidHaskellMain` from liquidhaskell-boot
    * `liquidhaskell-boot` uses user hooks:
        Looks up environment variable `LIQUID_DEV_MODE`
        If var exists, replace the build hook completely by `return ()`

rank2classes:
    doctests

hlibsass:
    done

HDBC-postgresql:
    pretty sure we've done it, but if we haven't, it's the same as postgrespl

aeson-diff:
    doctests

cuda:
  700 line Setup.hs
  ```haskell
    customHooks =
      simpleUserHooks
        { preBuild            = preBuildHook -- not using 'readHook' here because 'build' takes; extra args
        , preClean            = readHook cleanVerbosity
        , preCopy             = readHook copyVerbosity
        , preInst             = readHook installVerbosity
        , preHscolour         = readHook hscolourVerbosity
        , preHaddock          = readHook haddockVerbosity
        , preReg              = readHook regVerbosity
        , preUnreg            = readHook regVerbosity
        , postConf            = postConfHook
        , hookedPreProcessors = ("chs", ppC2hs) : filter (\x -> fst x /= "chs") preprocessors
        }
  ```
  Some parts look as though they should be in a configure script
  (`findCUDAInstallPath`, `generateAndStoreBuildInfo` (Runs CUDA detection procedure and stores .buildinfo to a file.), `validateLinker`, ...)

configuration-tools:
    done

Z-Data:
  ```haskell
  args <- getArgs
  if head args == "configure"
     then defaultMainArgs $ [ "--ghc-options", "-optcxx-std=c++11"
                            ] ++ args
     else defaultMain
  ```

cipher-aes128:
  Does configuration (search for C compiler) in build hook
  Sets args in gcc and ghc programs (updating LBI)
  ```haskell
  aesArgs = ["-mpclmul", "-maes", "-mssse3", "-DHAVE_AES_INTRINSICS", "-DWITH_AESNI"]
  ```

bitset:
    Pre-build it finds and runs `gcc` then runs the executable and writes the output to a header

lhs2tex:
    Complex 400 line `Setup.hs` script

bustle:
  ```haskell
  -- Okay, so we want to use hgettext's install hook, but not the hook that
  -- miraculously runs all our code through CPP just to add a couple of
  -- constants. (cpp doesn't like multi-line Haskell strings, so this is not
  -- purely an academic preference.)
  --
  -- Instead, we generate GetText_bustle.hs which contains the constants, in the
  -- same way as Paths_bustle.hs gets generated by Cabal. Much neater.
  --
  -- TODO: upstream this to hgettext
  installBustleHooks :: UserHooks
                     -> UserHooks
  installBustleHooks uh = uh
    { postInst = \a b c d -> do
          postInst uh a b c d
          GetText.installPOFiles a b c d
    , buildHook = \pkg lbi hooks flags -> do
          writeGetTextConstantsFile pkg lbi flags
          buildHook uh pkg lbi hooks flags
    }
  ```

network-bytestring:
  ```haskell
    runTests' _ _ _ lbi = do
        built <- doesDirectoryExist $ buildDir lbi
        unless built $ die "Run the 'build' command first."
        system "runhaskell -i./dist/build tests/Simple.hs"
        return ()
  ```

tlex-th:
    doctests

git-repair:
    Custom that is just Configure

newsynth:
    ```haskell
    import Distribution.Superdoc
    main = superdocMain
    ```

password-types:
    doctests

rzk:
    Custom Setup that runs bnfc to generate the language sub-libraries
    for the parsers included in Ogma.
    Run BNFC on the grammar before the actual build step.

hpage:
    Uses cabal-osx to install an app bundle if on macOS, changes postBuild,
    postInst, runTests

nvvm:
    Another 500 line Setup.hs akin to `cuda`

password-instances:
    doctests

proto-lens-protobuf-types:
    defaultMainGeneratingProtos "proto-src"

eternal:
    Custom that is Simple

tonatona:
    doctests

gf:
    Custom pre-build post-build pre-inst post-inst post-copy

tonatona-persistent-postgresql:
    doctests

ghc-boot:
    Generate GHC files in postConf

tonalude:
    doctests

encoding:
  ```haskell
   {hookedPreProcessors = (("mapping",\_ _ _ -> mappingPreprocessor)
                           :("mapping2",\_ _ _ -> mappingPreprocessor)
                           :("xml",\_ _ _ -> xmlPreprocessor)
                           :(hookedPreProcessors simpleUserHooks)
  ```

jsaddle-dom:
  Outputs a message every once in a while to reassure the user something is
  actually happening when compiling the massive package.
  ```haskell
  void . forkIO $
    forM_ [1..20] $ \n -> do
      threadDelay 60000000
      putStrLn $ "jsaddle-dom has been building for "
        <> show n <> " min (some modules are very large)."
      hFlush stdout
  buildHook simpleUserHooks a b c d
  ```
  Fun!

tonaparser:
    doctests

tonatona-servant:
    doctests

superdoc:
  * `superDocMain`
  ```haskell
  -- | This package extends Cabal's documentation building capabilities.
  -- It extends the Haddock markup language with syntax for subscripts,
  -- superscripts, and more. Recent version of Haddock already support
  -- bold text and the inclusion of images; however, we continue to
  -- provide markup for these for backward compatibility with earlier
  -- versions of Superdoc.
  --
  -- This package is designed to work transparently. It provides a
  -- custom main function that package maintainers can use in their
  -- @Setup.hs@ file.
  ```
  It's quite well documented


## Packages which are possibly worth migrating

* gtk2hs-buildtools source (not the Setup)

* ghc-mod (complex)

* liquidhaskell-boot, we are not able to migrate this. They probably use it to
    skip building altogether when testing liquid haskell
    ```haskell
    {- | This module provides a drop-in replacement for Cabal's 'defaultMain', to be used inside 'Setup.hs'
     modules of packages that wants to use the \"dev mode\". For more information, visit the documentation,
     especially the \"Developers' guide\".
    -}

    {-# LANGUAGE LambdaCase #-}
    module Language.Haskell.Liquid.Cabal (liquidHaskellMain) where

    import Distribution.Simple
    import System.Environment

    liquidHaskellMain :: IO ()
    liquidHaskellMain = do
      mbDevMode <- lookupEnv "LIQUID_DEV_MODE"
      defaultMainWithHooks (devModeHooks mbDevMode)

    devModeHooks :: Maybe String -> UserHooks
    devModeHooks = \case
      Nothing               -> simpleUserHooks
      Just x | x == "false" -> simpleUserHooks
      Just _                -> simpleUserHooks { buildHook = \_ _ _ _ -> return () }
    ```
* cuda
* cipher-aes128
* lhs2tex
* bustle
* nvvm, but it should suffice to do either cuda or this one.
* gf

## Stats

Total packages:                16091

Packages fetched successfully: 16091

Packages build-type: Custom:   543

Manually inspected from /top:  100
