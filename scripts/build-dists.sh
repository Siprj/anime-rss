#!/bin/sh -e

source="$(git rev-parse --show-toplevel)"
cd "$source/"

cabal update
cabal configure --enable-optimization --disable-executable-dynamic
cabal build anime-rss
cabal build admin-tui
mkdir -p ./anime-rss-dist/bin/
cp $(cabal list-bin anime-rss) ./anime-rss-dist/bin/
cp $(cabal list-bin admin-tui) ./anime-rss-dist/bin/

mkdir -p ./anime-rss-dist/frontend
( cd frontend
  npm ci
  npx elm make --output=elm.js src/Main.elm
  npx uglifyjs elm.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' | npx uglifyjs --mangle --output elm.min.js
  cp elm.min.js ../anime-rss-dist/frontend/
  cp index.html ../anime-rss-dist/frontend/
  sed -i 's/elm.js/elm.min.js/g' ../anime-rss-dist/frontend/index.html
  cp -r css ../anime-rss-dist/frontend/css/
  cp style.css ../anime-rss-dist/frontend/style.css
  cp loading.png ../anime-rss-dist/frontend/loading.png
  cp -r webfonts ../anime-rss-dist/frontend/webfonts/
)
