module Access exposing (..)

import Entity exposing (..)


-- User access
-- g - Get


userName : List Entity.User -> String -> Maybe Entity.User
userName users userid =
    List.filter (\u -> u.id == userid) users
        |> List.head
