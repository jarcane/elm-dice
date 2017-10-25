#!/bin/sh
CURRENTDATE="date +'%D'"

elm-make Main.elm --output=elm.js
git commit -am "Deployment - $CURRENTDATE"
git push