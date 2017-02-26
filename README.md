# Craving Control Frontend

To run in development, first ensure `craconapi` is running, then compile:

 - cd `src/`
 - `elm make Main.elm --output ../dist/js/app.js --debug --warn`
 - Then `cd ../dist` and run `alfred` or something to serve the files
 - visit http://localhost:4001/

This can be nice: `while :; do clear; your_command; sleep 2; done`
