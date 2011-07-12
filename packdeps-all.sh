#!/bin/sh

for package in web-routes web-routes-boomerang web-routes-happstack web-routes-hsp web-routes-regular web-routes-th web-routes-wai
do
  packdeps $package/$package.cabal
done

