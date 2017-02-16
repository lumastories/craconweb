### Build

`elm make Main.elm --output ../build/app.html`

### Develop

The http server will run on :8680. See `/swagger` for docs.


### Files

`Main.elm` is our entry file. Here, we import everything from the other files and actually connect everything together. We also setup ports for interop with JS in this file. We run elm-make on this file to generate a JS file that we can include elsewhere.

In `Model.elm`, we contain the actual model for the view state of our program. Sometimes, we include decoders for our model in here.

`Update.elm` contains our update code. This includes the Msg types for our view. Inside here most of our business logic lives.

Inside `View.elm`, we define the view for our model and set up any event handlers we need.


## TODO
POST {username:"",password""} to /auth
Store the token
Navigate to /#games
GET /user/:id (get user id from JWT payload, pass JWT as Authorization HTTP headers)
 