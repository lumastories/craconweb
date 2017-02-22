# Craving Control Frontend

To run in development, first ensure `craconapi` is running, then compile:

 - cd `src/`
 - `elm make Main.elm --output ../dist/js/app.js --debug --warn`
 - Then `cd ../dist` and run `alfred` or something to serve the files
 - visit http://localhost:4001/

This can be nice: `while :; do clear; your_command; sleep 2; done`

Alternatively

 - `npm run build`

# Dev dependencies

 - npm
 - ruby -> gem -> sass (`sudo gem install sass`)
 - 



## Todo

*do*
- [ ] Unpack jwt, display user info on games page
- [ ] Extract material design, use bulma css instead
- [ ] Make the main menu with bulma
- [ ] Figure out why `/#games` goes to login `/` initially. por que?
- [ ] Create a new logo (brain with CC - craving control, crave control)
- [ ] Select a color pallette (check color blind compatability)
- [ ] Create reset password modal
- [ ] Write decoders and encoders for swagger 
- [ ] Auto generate models, encoders and decoders from swagger (whaaaa) or use IDL or GRPC -> elm
- [ ] Rename the `dist` directory to something more fitting, check NoRedInk guidelines?




*done*
- [x] Mockup the "Admin" frontend (the "back")
- [x] Establish dir structure
- [x] Establish build tooling
- [x] Read and adhere to [NoRedInk Style Guide](https://github.com/NoRedInk/elm-style-guide)
- [x] [read about jwt](http://package.elm-lang.org/packages/simonh1000/elm-jwt/4.0.3)
- [x] Make a simple request
- [x] [read about mdl](http://package.elm-lang.org/packages/debois/elm-mdl/8.1.0)
- [x] Make basic layout
- [x] Establish dev process for continous building (`craca` and `crac` aliass. `api` to run the go api)
- [x] Establish dev process for accessing Go API on localhost
- [x] Establish routes and read [Navigation.elm docs](http://package.elm-lang.org/packages/elm-lang/navigation/2.1.0)
- [x] Create initial models for authentiaction
- [x] Create login view 
- [x] Read some NoRedInk [blogs](http://tech.noredink.com/)


