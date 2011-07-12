#!/bin/sh
# sudo apt-get remove --purge libghc6-happstack-facebook-prof libghc6-happstack-facebook-dev libghc6-web-routes-mtl-prof libghc6-web-routes-mtl-dev libghc6-web-routes-hsp-prof libghc6-web-routes-hsp-dev libghc6-web-routes-happstack-prof libghc6-web-routes-happstack-dev libghc6-web-routes-prof libghc6-web-routes-dev

cd web-routes           && fakeroot debian/rules clean && cd .. && \
cd web-routes-th        && fakeroot debian/rules clean && cd .. && \
cd web-routes-hsp       && fakeroot debian/rules clean && cd .. && \
cd web-routes-mtl       && fakeroot debian/rules clean && cd .. && \
cd web-routes-happstack && fakeroot debian/rules clean && cd ..
