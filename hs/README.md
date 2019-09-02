Reach Compiler
=================

This is the Reach compiler, itself written in Haskell.

Installing Dependencies
-----------------------

First, install [stack](https://www.haskellstack.org/) and [z3](https://github.com/Z3Prover/z3).

Using [Nix](http://nixos.org/nix) or [NixOS](http://nixos.org/) with the 'nixpkgs-unstable' channel,
you just need to install stack, and stack will use Nix to install z3:

    nix-channel --add https://nixos.org/channels/nixpkgs-unstable
    nix-channel --update
    nix-env -f '<nixpkgs>' -iA stack

On Ubuntu or Debian, you would use the following, except that as of June 11th 2019,
Debian only provides z3 4.4.1 whereas we depend on features from z3 4.8.5,
so you'll instead have to install z3 from source or binary release
(or build using nix with `stack --nix`):

    apt install haskell-stack libz3-dev

Instead, download a release zip file from < https://github.com/Z3Prover/z3/releases >
and put the `bin/` directory in your `PATH`, e.g. using `stow`.


Building and Testing the Reach Compiler
------------------------------------------

Once your dependencies are installed, use:

    stack test

Use `stack --nix test` to force use of Nix on e.g. Debian, macOS, etc.

You can use `stack build` instead of `stack test`
if you want to build the software without running the tests.

