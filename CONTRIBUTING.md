# Contributor guide

## About this document

This guide is for people who would like to be involved in building Stainless.

This guide assumes that you have some experience doing Scala
development. If you get stuck on any of these steps, please feel free
to [ask for help](#getting-in-touch).

## How can I help?

Stainless follows a standard
[fork and pull](https://help.github.com/articles/using-pull-requests/)
model for contributions via GitHub pull requests.

### Build the project

First you'll need to checkout a local copy of the code base:

```sh
git clone git@github.com:epfl-lara/stainless.git
```

To build Stainless you should have
[sbt](https://www.scala-sbt.org/1.x/docs/Setup.html) 1.2.8 installed. If you'd like, you can use the [Nix Stainless development environment](#nix-stainless-development-environment).

 Run `sbt`, and then use any of the following commands:

 * `compile`: compile the code
 * `test`: run the tests
 * `it: test`: run the integration tests

### Attributions

If your contribution has been derived from or inspired by other work, please
state this in its ScalaDoc comment and provide proper attribution. When
possible, include the original authors' names and a link to the original work.

### Submit a pull request

If your pull request addresses an existing issue, please tag that
issue number in the body of your pull request or commit message. For
example, if your pull request addresses issue number 52, please
include "Fixes #52".

## Getting in touch

Discussion around Stainless is currently happening in the
[Gitter channel](https://gitter.im/epfl-lara/stainless) as well as on Github
issue and PR pages.

Feel free to open an issue if you notice a bug, have an idea for a
feature, or have a question about the code. Pull requests are also
gladly accepted.

People are expected to follow the
[Scala Code of Conduct](https://www.scala-lang.org/conduct/) when
discussing Stainless on the Github page, Gitter channel, or other
venues.

We hope that our community will be respectful, helpful, and kind. If
you find yourself embroiled in a situation that becomes heated, or
that fails to live up to our expectations, you should disengage and
contact one of the [project maintainers](#maintainers)
in private. We hope to avoid letting minor aggressions and misunderstandings
escalate into larger problems.

If you are being harassed, please contact one of [us](#maintainers)
immediately so that we can support you.

## Nix Stainless Development Environment

Since Stainless development can include the Scala runtime, the Scala.js runtime, the Stainless website, and more; a number of dependencies (sbt, Node.js, Jekyll, etc) can be needed to work on Stainless. Managing these dependencies globally can be a hassle and can lead to version conflicts. To make this easier to manage in an isolated development environment, Stainless provides a `bin/stainless.nix` for anyone using the [Nix package manager](https://nixos.org/nix/).

To use the Nix-based Stainless development environment:

1. [Install](https://nixos.org/nix/download.html) the Nix package manager.
2. At the root level of the Stainless repository, run `nix-shell --pure bin/stainless.nix`. This will drop you into a minimal bash shell that has just the required dependencies on the `PATH`. Note that the first time that you run this it will take some extra time to download the necessary dependencies into your local Nix store.
3. Run `sbt`, etc. as required from the `nix-shell`.
4. When you are finished you can `exit` the `nix-shell`.

## Maintainers

TODO
