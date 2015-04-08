with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, bytestring, happstack-server, stdenv, text
             , web-routes
             }:
             mkDerivation {
               pname = "web-routes-happstack";
               version = "0.23.9";
               src = ./.;
               buildDepends = [
                 base bytestring happstack-server text web-routes
               ];
               description = "Adds support for using web-routes with Happstack";
               license = stdenv.lib.licenses.bsd3;
             }) {};
in
  pkg.env
