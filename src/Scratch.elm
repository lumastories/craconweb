<nav class="nav has-shadow"><div class="container"><div class="nav-left"><a class="nav-item" href="/"><img src="img/logo.png"></a><a class="nav-item is-tab is-active is-hidden-mobile" href="/">Home</a><a class="nav-item is-tab is-hidden-mobile" href="/badges">Badges</a><a class="nav-item is-tab is-hidden-mobile" href="/instructions">Instructions</a><a class="nav-item is-tab is-hidden-mobile" href="/settings">Settings</a></div><span class="nav-toggle"><span></span><span></span><span></span></span><div class="nav-right nav-menu"><a class="nav-item is-tab">Log out</a><a class="nav-item is-tab is-active is-hidden-tablet" href="/">Home</a><a class="nav-item is-tab is-hidden-tablet" href="/badges">Badges</a><a class="nav-item is-tab is-hidden-tablet" href="/instructions">Instructions</a><a class="nav-item is-tab is-hidden-tablet" href="/settings">Settings</a></div></div></nav>


navLink : String -> R.Route -> ( R.Route, String ) -> String -> Html Msg
navLink text_ activeRoute ( route, linkPath ) hidden =
    let
        class_ =
            if route == activeRoute then
                "nav-item is-tab is-active " ++ hidden
            else
                "nav-item is-tab " ++ hidden

        onClick_ =
            R.onLinkClick <| UpdateLocation linkPath
    in
        


