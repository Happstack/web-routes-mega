{ mkDerivation, base, HUnit, parsec, QuickCheck, split, stdenv
, template-haskell, test-framework, test-framework-hunit
, test-framework-quickcheck2, test-framework-th, text, web-routes
}:
mkDerivation {
  pname = "web-routes-th";
  version = "0.22.2";
  src = ./.;
  buildDepends = [
    base parsec split template-haskell text web-routes
  ];
  testDepends = [
    base HUnit QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2 test-framework-th web-routes
  ];
  description = "Support for deriving PathInfo using Template Haskell";
  license = stdenv.lib.licenses.bsd3;
}
