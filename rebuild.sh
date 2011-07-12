#!/bin/sh
sudo apt-get remove --purge libghc6-happstack-facebook-prof libghc6-happstack-facebook-dev libghc6-web-routes-mtl-prof libghc6-web-routes-mtl-dev libghc6-web-routes-hsp-prof libghc6-web-routes-hsp-dev libghc6-web-routes-happstack-prof libghc6-web-routes-happstack-dev libghc6-web-routes-prof libghc6-web-routes-dev

cd web-routes && dpkg-buildpackage -b -us -uc -rfakeroot && sudo debi && cd .. && \
cd web-routes-th && dpkg-buildpackage -b -us -uc -rfakeroot && sudo debi && cd .. && \
cd web-routes-hsp && dpkg-buildpackage -b -us -uc -rfakeroot && sudo debi && cd .. && \
cd web-routes-mtl && dpkg-buildpackage -b -us -uc -rfakeroot && sudo debi && cd .. && \
cd web-routes-happstack && dpkg-buildpackage -b -us -uc -rfakeroot && sudo debi && cd ..
# cd web-routes-wai && dpkg-buildpackage -b -us -uc -rfakeroot && sudo debi && cd ..
