### Build

`elm make Main.elm --output ../build/app.html`


### Test

Currently we only have dummy tests. But the framework is in place.

`npm install -g elm-test`

Then run `elm-test`

### Develop

The http server will run on :8680. See `/docs` for docs.

### Entities

1. pb2elm ~/work/src/gitlab.com/lumastudio/craconidl/cracon.proto




### Files

`Main.elm` is our entry file. Here, we import everything from the other files and actually connect everything together. We also setup ports for interop with JS in this file. We run elm-make on this file to generate a JS file that we can include elsewhere.

In `Model.elm`, we contain the actual model for the view state of our program. Sometimes, we include decoders for our model in here.

`Update.elm` contains our update code. This includes the Msg types for our view. Inside here most of our business logic lives.

Inside `View.elm`, we define the view for our model and set up any event handlers we need.

 
## Notes

Navigation.Location...

```
type alias Location = 
    { href : String
    , host : String
    , hostname : String
    , protocol : String
    , origin : String
    , port_ : String
    , pathname : String
    , search : String
    , hash : String
    , username : String
    , password : String
    }
```
