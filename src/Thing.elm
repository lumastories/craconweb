module Thing exposing (..)

import Time


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
    }



-- TODO change initialGames data based on freshly flattened type


initialGames : List Game
initialGames =
    [ { id = ""
      , slug = "gn"
      , name = "Go/No-Go"
      , about = """During this game, you will see a series of pictures on the screen surrounded by a solid or a dashed border.
Your job is to press "c" if the image is on the left side of the screen and "m" if the image is on the right side of the screen.
Only press if there is a solid bar around the image! If you see a dashed bar, do nothing and the next photo will appear shorty!
Do your best to respond as quickly and accurately as possible. Press any key to begin."""
      , icon = "img/icons3.png"
      , durIncrement = 0
      , durDecrement = 0
      , duration = 1250
      , trialCount = 600
      , blockCount = 2
      , incTrigger = 0
      , decTrigger = 0
      , interval = 500
      , fixCross = False
      }
    , { id = ""
      , slug = "vs"
      , name = "Visual Search"
      , about = """During this game, you will see a grid of 16 photographs of models. It is your job to click on the image of the average weight woman as quickly as you can. This is a tough task! She will be surrounded by ultra-thin women.
Do your best to respond as quickly and accurately as possible."""
      , icon = "img/icons1.png"
      , durIncrement = 0
      , durDecrement = 0
      , duration = 1250
      , trialCount = 600
      , blockCount = 2
      , incTrigger = 0
      , decTrigger = 0
      , interval = 500
      , fixCross = True
      }
    , { id = ""
      , slug = "dp"
      , name = "Dot Probe"
      , about = """In this game, you will see two pictures on the screen. The pictures will disappear and a dot will replace one of the two photos.
Your job is to press the "c" if the dot appears behind the left photograph or "m" if the dot appears behind the right photograph.
Do your best to respond as quickly and accurately as possible.
Press any key to begin"""
      , icon = "img/icons2.png"
      , durIncrement = 0
      , durDecrement = 0
      , duration = 1250
      , trialCount = 600
      , blockCount = 2
      , incTrigger = 0
      , decTrigger = 0
      , interval = 500
      , fixCross = True
      }
    , { id = ""
      , slug = "ss"
      , name = "Stop Signal"
      , about = """Images will be displayed on the screen in either dark blue or light gray border. Press the spacebar as quickly as possible when you see the blue border and withhold responding if the border is gray."""
      , icon = "img/icons4.png"
      , durIncrement = 0
      , durDecrement = 0
      , duration = 1250
      , trialCount = 600
      , blockCount = 2
      , incTrigger = 0
      , decTrigger = 0
      , interval = 500
      , fixCross = False
      }
    , { id = ""
      , slug = "rs"
      , name = "Respond Signal"
      , about = """k"""
      , icon = "img/icons5.png"
      , durIncrement = 0
      , durDecrement = 0
      , duration = 1250
      , trialCount = 600
      , blockCount = 2
      , incTrigger = 0
      , decTrigger = 0
      , interval = 500
      , fixCross = True
      }
    ]


type alias Stimulus =
    { src : String
    , isValid : Bool
    }


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


type alias GameAction =
    { time : Time.Time
    , initialTime : Time.Time
    , ommision : Bool
    , commision : Bool
    , correct : Bool
    , stimulus : Stimulus
    }



--Stimuli display duration (milliseconds)
--Stimuli display onset (milliseconds)
--Player response time (milliseconds)
--Ommision errors
--Commision errors
--Correct / Incorrect
--Task Completion / How long they played (with partial data)
--Condition (control or experimental)
--Login date / time
--NEW - which specific images were seen / selected.
--design training wisely
--vs, dot probe - attention - pay attention to healthy food
--other 3 - response training - increase valuation of healthy food
--attention networks v. flanker task
--better, more and variety
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
