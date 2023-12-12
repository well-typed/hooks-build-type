This repository contains documents produced by
[Well-Typed](https://well-typed.com/) while working on improvements to `Cabal`,
thanks to investment from the [Sovereign Tech
Fund](https://www.sovereigntechfund.de/) Contribute Back Challenges.
For a high level overview,  see our blog post announcing the project:
[Sovereign Tech Fund invests in Cabal as critical Haskell infrastructure](https://www.well-typed.com/blog/2023/10/sovereign-tech-fund-invests-in-cabal/).


## Survey of uses of the `Custom` build-type

We initially carried out a survey of packages using the `Custom` build-type
based on the Stackage LTS 21.9 package set. [Results of this
survey](./survey.md) were used to inform the initial proposal design
and prototype implementation.  We then wrote
[experimental patches for surveyed packages](https://gitlab.haskell.org/mpickering/hooks-setup-testing/-/tree/a664eb3cf456ad8e6e4f922406f1f904575339b5/patches)
and summarised the [uses for hooks in each package](./hooks-uses.md).

In subsequent discussion of the design it became apparent that it would be
useful to consider a wider range of Haskell packages which might exercise corner
cases, so we [expanded the survey](./extended-survey.md) to look at packages
across Hackage.


## Design

 * [Early draft design](./design.md) discussed with Cabal developers prior to HF Tech Proposal submission

 * [HF Tech Proposal as initially submitted](https://github.com/adamgundry/tech-proposals/blob/a5aa0cdfdb5a0283bcbe765b671038c82fc3797f/proposals/0000-replacing-cabal-custom-build.md)

 * [Discussion of the proposal](https://github.com/haskellfoundation/tech-proposals/pull/60)

 * [Current state of the proposal following revisions](https://github.com/adamgundry/tech-proposals/blob/replacing-cabal-custom-build/proposals/0000-replacing-cabal-custom-build.md)


## Implementation

 * [Prototype implementation of the original HF tech proposal design](https://github.com/mpickering/cabal/tree/wip/setup-hooks-v1)

 * [Current state of the implementation following revisions](https://github.com/mpickering/cabal/commits/wip/setup-hooks)

 * [Implementation PRs from October-December 2023](https://github.com/search?q=repo%3Ahaskell%2Fcabal+is%3Apr+author%3Aalt-romes+author%3Asheaf+author%3Ampickering+created%3A2023-09-01..2023-12-31&type=pullrequests)
