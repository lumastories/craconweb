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
    { id : String
    , slug : String
    , name : String
    , about : String
    , icon : String
    , durIncrement : Int
    , durDecrement : Int
    , duration : Int
    , trialCount : Int
    , blockCount : Int
    , incTrigger : Int
    , decTrigger : Int
    , interval : Int
    , fixCross : Bool
    , stimuli : List Stimulus
    }



-- TODO change initialGames data based on freshly flattened type


initialGames : List Game
initialGames =
    [ { id = ""
      , slug = "gn"
      , name = "Go/No-Go"
      , about = "Don't hesitate. This game is very interesting. Here is even more info about the game"
      , icon = "img/icons3.png"
      , durIncrement = -1
      , durDecrement = -1
      , duration = 1250
      , trialCount = 400
      , blockCount = 8
      , incTrigger = -1
      , decTrigger = -1
      , interval = 500
      , fixCross = False
      , stimuli = someStims
      }
    , { id = ""
      , slug = "vs"
      , name = "Visual Search"
      , about = "Find the right thing. This game is very interesting. Here is even more info about the game"
      , icon = "img/icons1.png"
      , durIncrement = -1
      , durDecrement = -1
      , duration = 1250
      , trialCount = 400
      , blockCount = 8
      , incTrigger = -1
      , decTrigger = -1
      , interval = 500
      , fixCross = False
      , stimuli = someStims
      }
    , { id = ""
      , slug = "dp"
      , name = "Dot Probe"
      , about = "Watch the dot. This game is very interesting. Here is even more info about the game"
      , icon = "img/icons2.png"
      , durIncrement = -1
      , durDecrement = -1
      , duration = 1250
      , trialCount = 400
      , blockCount = 8
      , incTrigger = -1
      , decTrigger = -1
      , interval = 500
      , fixCross = False
      , stimuli = someStims
      }
    , { id = ""
      , slug = "ss"
      , name = "Stop Signal"
      , about = "Watch for the signal! This game is very interesting. Here is even more info about the game"
      , icon = "img/icons4.png"
      , durIncrement = -1
      , durDecrement = -1
      , duration = 1250
      , trialCount = 400
      , blockCount = 8
      , incTrigger = -1
      , decTrigger = -1
      , interval = 500
      , fixCross = False
      , stimuli = someStims
      }
    , { id = ""
      , slug = "rs"
      , name = "Respond Signal"
      , about = "Respond to a signal! This game is very interesting. Here is even more info about the game"
      , icon = "img/icons5.png"
      , durIncrement = -1
      , durDecrement = -1
      , duration = 1250
      , trialCount = 400
      , blockCount = 8
      , incTrigger = -1
      , decTrigger = -1
      , interval = 500
      , fixCross = False
      , stimuli = someStims
      }
    ]


type alias Stimulus =
    { src : String
    , isValid : Bool
    }



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


someStims =
    [ Stimulus "img/dev/i1.png" False
    , Stimulus "img/dev/i2.png" False
    , Stimulus "img/dev/i3.png" False
    , Stimulus "img/dev/i4.png" False
    , Stimulus "img/dev/v1.png" True
    , Stimulus "img/dev/v2.png" True
    , Stimulus "img/dev/v3.png" True
    , Stimulus "img/dev/v4.png" True
    ]
