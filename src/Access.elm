module Access exposing (..)

import Entity exposing (..)


findUser : List Entity.User -> String -> Maybe Entity.User
findUser users userid =
    List.filter (\u -> u.id == userid) users
        |> List.head


names : List { record | names : a } -> List a
names =
    List.map .names
