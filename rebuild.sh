#!/bin/sh
sudo apt-get remove --purge libghc-happstack-facebook-prof libghc-happstack-facebook-dev libghc-web-routes-mtl-prof libghc-web-routes-mtl-dev libghc-web-routes-hsp-prof libghc-web-routes-hsp-dev libghc-web-routes-happstack-prof libghc-web-routes-happstack-dev libghc-web-routes-prof libghc-web-routes-dev

for package in web-routes web-routes-th web-routes-hsp web-routes-happstack ; do
 cd $package
 echo "building $package..."
 dpkg-buildpackage -b -us -uc -rfakeroot && sudo debi
 cd ..
done

echo "rebuild.sh done."
