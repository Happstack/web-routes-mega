with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, hsp, stdenv, text, web-routes }:
             mkDerivation {
               pname = "web-routes-hsp";
               version = "0.24.5";
               src = ./.;
               buildDepends = [ base hsp text web-routes ];
               description = "Adds XMLGenerator instance for RouteT monad";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
