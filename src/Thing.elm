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
    }


initialGames : List Game
initialGames =
    [ { slug = "vs"
      , icon = "img/icons1.png"
      , name = "Visual Search"
      , about = "Find the right thing. This game is very interesting. Here is even more info about the game"
      }
    , { slug = "dp"
      , icon = "img/icons2.png"
      , name = "Dot Probe"
      , about = "Watch the dot. This game is very interesting. Here is even more info about the game"
      }
    , { slug = "gn"
      , icon = "img/icons3.png"
      , name = "Go/No-Go"
      , about = "Don't hesitate. This game is very interesting. Here is even more info about the game"
      }
    , { slug = "ss"
      , icon = "img/icons4.png"
      , name = "Stop Signal"
      , about = "Watch for the signal! This game is very interesting. Here is even more info about the game"
      }
    , { slug = "rs"
      , icon = "img/icons5.png"
      , name = "Respond Signal"
      , about = "Respond to a signal! This game is very interesting. Here is even more info about the game"
      }
    ]
