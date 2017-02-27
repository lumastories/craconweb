-- Snippets.elm


module Main exposing (..)


encodeUser : User -> Json.Encode.Value
encodeUser user =
    Json.Encode.object
        [ ( "id", Json.Encode.string <| user.id )
        , ( "username", Json.Encode.string <| user.username )
        , ( "email", Json.Encode.string <| user.email )
        , ( "firstName", Json.Encode.string <| user.firstName )
        , ( "lastName", Json.Encode.string <| user.lastName )
        ]


cmdForEnterOnPage : Model -> ( Bool, Cmd Msg )
cmdForEnterOnPage model =
    case model.activePage of
        Login ->
            ( True, Http.send LoginResponse (postCreds model) )

        _ ->
            ( False, Cmd.none )
