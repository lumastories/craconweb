



SetGreeting t ->
    let
        newGreeting =
            if (Date.fromTime t |> Date.hour) < 12 then
                "Good morning."
            else
                "Good evening."
    in
        ( { model | greeting = newGreeting }, Cmd.none )
