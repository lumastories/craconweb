module Thing exposing (..)


type alias User =
    { id : String
    , email : String
    , username : String
    , firstName : String
    , lastName : String
    }


initialUser : String -> User
initialUser firstName =
    { id = ""
    , username = ""
    , email = ""
    , firstName = firstName
    , lastName = ""
    }


type alias Game =
    { slug : String
    , icon : String
    , name : String
    , about : String
    , data : GameData
    }


initialGames : List Game
initialGames =
    [ { slug = "vs"
      , icon = "img/icons1.png"
      , name = "Visual Search"
      , about = "Find the right thing. This game is very interesting. Here is even more info about the game"
      , data = goNoGoGameData
      }
    , { slug = "dp"
      , icon = "img/icons2.png"
      , name = "Dot Probe"
      , about = "Watch the dot. This game is very interesting. Here is even more info about the game"
      , data = goNoGoGameData
      }
    , { slug = "gn"
      , icon = "img/icons3.png"
      , name = "Go/No-Go"
      , about = "Don't hesitate. This game is very interesting. Here is even more info about the game"
      , data = goNoGoGameData
      }
    , { slug = "ss"
      , icon = "img/icons4.png"
      , name = "Stop Signal"
      , about = "Watch for the signal! This game is very interesting. Here is even more info about the game"
      , data = goNoGoGameData
      }
    , { slug = "rs"
      , icon = "img/icons5.png"
      , name = "Respond Signal"
      , about = "Respond to a signal! This game is very interesting. Here is even more info about the game"
      , data = goNoGoGameData
      }
    ]


type alias GameData =
    { id : String
    , name : String
    , slug : String
    , durIncrement : Int
    , durDecrement : Int
    , duration : Int
    , trialCount : Int
    , blockCount : Int
    , incTrigger : Int
    , decTrigger : Int
    , interval : Int
    , fixCross : Bool
    }



{--



  --}
-- TODO augment game data model for stop-signal to include response frame duration
{--
  Stair casing: increase or decrease
  speed by durInccrement/durDecrement
  based on previous RT
  default duration
  how many trials to display
  in how many blocks
  criteria for stair casing
  respond after incTrigger milliseconds
  how long between trials
  wether to show a fixation cross during interval
  --}


goNoGoGameData =
    { id = ""
    , name = "Go/No-Go"
    , slug = "gonogo"
    , durIncrement = -1
    , durDecrement = -1
    , duration = 1250
    , trialCount = 400
    , blockCount = 8
    , incTrigger = -1
    , decTrigger = -1
    , interval = 500
    , fixCross = False
    }
