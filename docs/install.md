# Installation guide

/under construction/

## From latest sources on Ubuntu 16.04 using stack

  * Install stack (Haskell build tool).

    * Use `curl` or `wget` following the [stack installation instructions](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

    * Add `/usr/local/bin` to your path or create a symbolic link to `/usr/local/bin/stack` somewhere on your path, eg as follows:

      > cd; mkdir -p bin; ln -s /usr/local/bin/stack bin/stack

    * Check stack is installed by running:

      > stack --version

        * This should report version 1.5 or newer.

  * Setup stack and install [ghc](https://www.haskell.org/ghc/) (Haskell compiler) 7.10.3 (unless already installed).

    > sudo apt-get install build-essential

    > stack setup --resolver lts-6.5

      * This command is likely to take several minutes to complete and download a large amount of data.

      * Prefer a newer resolver?  AERN2 is likely to work with newer resolvers, but beware that in ghc 8.0.2 AERN2 does not work with ghci due to a ghci bug.

    * Check your installation by running:

      > stack ghc -- --version

      * This should report ghc version 7.10.3.

  * Clone mixed-types-num and aern2 git repositories as follows:

    * In a new folder, run: TODO
