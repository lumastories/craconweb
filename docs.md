There is a module for each game and each shares the same structure. They represent the transformations and state required for executing each game. The each have the following elements:

type alias Trial - A record which holds all of the individual state of any trial. In here is information like the urls of the trial, whether it is Go or NoGo, the stage stage of the trial (finite state machine) and a place for the trial to store any reporting information.

type alias Settings - A record which is used out in the world with configuration needed to construct and run this particular trial. This includes things like the numbers which configure how many trials get created in what ratios and how much time to spend on each stage of the trial. This record is *stateful* between trials. Each event callback function (described belowe) takes and returns a `Settings`, so here is where you can stash information to be used by the next trial to execute differently. This is where staircasing goes.

init - A function which builds a `Generator ( List (List Trial) )` from settings and some lists of Urls. This is the toughest-to-grok code I think I have written here. There is a lot of `Generator` composition here to handle all of the random elements that trial construction requires.

That said, they are already very close to spec and you should only need to tweak them most of the time. The key thing to look for is any call to `initTrial`, this is where the url is being turned into a local `Trial` type and where any additional trial information should go.

initTrial - Takes what is required to build a `Trial`. Only called by `init`, but worth mentioning.

trialFuns - This is a configuration which is required for the `GameManager` which contains all of the callback functions for the trial. It simply references its internal callbacks or generic noOps from `GenGame`. These are the rest of the API for a trial. The rest of this will talk about these functions.

getTrialImages - I should have removed this. It was actually part of an experiment to try doing preloads between trials. It prompted the refactor which produced the need for `trialFuns` and accidentally got left. It is currently a default function which returns an empty list.

updateTime, updateIndication - These each take `Settings` the current `Trial` and perhaps some other information like `Time` `Direction` or `Int` and use that to decide how to change the `Trial` and `Settings` for the next run. They basically just interpret the input (including time ticks) and update the state as required. They return a `TrialResult` which is usually either `Continuing Trial` which indicates to continue running the trial or `Complete (Maybe Reason)` which indicates to stop (there is also `ContinuingWithEvent trial (Cmd msg)` which is only used for the 'ding' in Respond Signal).

Looking back, I should have incorporated the return of `Settings` into `TrialResult`. Regardless, this is where results logging can occur. The current `Reason` data structure is inadequate for the purpose, but could be replaced by something else.

view - Takes a `Trial` and perhaps some extra configuration and returns `Html msg`. This means that if you want to show it in your view, it needs to be in the `Trial` or somehow injected from above (see Respond Signal for an example of this).


The GenGame module contains shared types and functions between all of these files. It's mostly just boring helper functions.

The GameManager module is where the shared functionality of trials is performed. It is also where most of the interaction with the outside app occurs.

GameStatus - Like `TrialResult`, it is indicating to the world above whether the game is complete or not, this duplication has some smell to it and should indicate that things could be simpler. Something of this type is returned from all of the callbacks later on.

GameData - This record configures the running of the entire game. The names are reasonably descriptive, I think. `results` and `settings` are built from the responses of the trial callbacks.

InitConfig - A record to be used outside and handed to `init` with all of the information needed to build a `GameData` (excepting any randomization).

Trials - Holds either the Trial or is a Rest. The GameManager does handle the rests between trials.

Blocks - From when I thought that the spec still included Blocks, we have this construction. The Block is either a rest, the current list of blocks of trials, the Instructions page or the Report page.

init - Builds a GameData. Since jitter is random, it returns a Generator to inject the randomness.

padBlocks - Like the trial init functions, this is a bit dense to read. Names for subfunctions would be nice here, but I just can't seem to figure them out in a way which doesn't just fragment things even further.

updateTime, updateTimeHelper, updateDirectionIndication, updateIndication, updateIntIndication, updateIndicationHelper - These are functions which just set up the call to `updateHelper` They are a bit noisy to read and could probably be trimmed down a bit, particularly since the last refactor of this code, but I haven't spent time doing so. In particular, they select the correct function from `trialFuns` and apply them to the particular arguments they need (e.g. `Time` or `Direction`).

dismissInfo - This allows the generic "Indication" event (currently just keyboard presses) to dismiss the Instructions and end-of-session Report. It also sets the start of the session time to the time of that indication.


updateHelper - It looks big and scary, but it is not that bad. The `updateF` parameter is a function from `trialFuns` which is applied down to just requiring the `trial` (yay currying). There are some functions defined in the let block which I will explain.

reRun - This gets called whenever something realizes that things should advance, ether a block or a trial. The benefit is that we don't have to wait for another message to do the advancement. This also enforces the duration limit such that any current trial will complete.

noOp - Don't do anything. This is used for Instructions and Report to stay until a key is pressed.

durationSwitch - Helper for checking if a duration is up and doing a `reRun` if it is or nothing extra if it isn't.

The case statements are just looking at the `remainingBlocks` and deciding what to do. If it finds an active trial, it applies `updateF` to it. Note that this function returns a `Cmd msg` as well, so if you wanted to, say, record the results of every trial, it could be done from here. It was by writing this function that I learned the most about the problem space.

view - dispatch to the correct view.

There is also some code in Update which should be explained.

handle*Update - This code does not make me proud, but I think the refactor it enabled is worthwhile. If `GameManager.update` taught me a bunch about the problem, this teaches some of the value of improving upon it. Basically, the problem is that the `Settings` and `Trial` records for each of the games is different. If these types could be aligned, then the `Game` type wouldn't need to exist and this messy dispatching where we deconstruct and reconstruct `Game`s would go away. I will refer back to this in a moment, but these functions standardize making the callback, interpreting the result, updating the model, and issuing any `Cmd`s that are produced (like the tone in Respond Signal).

handleGameInit - To construct an `InitConfig` (which is used to construct a `GameData` via `GameManager.init`), we need *both* the current time and the randomizations. This requires making the `Random.Generator` a `Task` so that it can compose with `Time.now`. This required writing `GenGame.generatorToTask`. This is an important function for constructing repeatable trials based on fixed seeds. These calls to `generatorToTask` are causing things to be non-deterministic and would need to be replaced with `Random.step` and `Random.initialSeed` (probably wrapped up in a function) to make them repeatably deterministic.

applyImages - I hate this name, but it handles all of the vagaries of building the various init commands in a repeatable way.

There are also some `Msg`s handled in `Update` worth mentioning.

You seem to know about the `Init` game messages and there is little to talk about here.

Presses, IntIndication, NewCurrentTime - each call into their respective `GameManager` functions via `handle*Update` functions.

FillerResp, ValidResp, InvalidResp - These are currently triggering a preload for *all* of the images. This is proof-of-concept, but could probably be improved. Setting `onLoad` events for the `Image` objects which call an incoming Elm port to record which images have loaded and which haven't would be nice and allow for that 'loading' screen we had discussed.

I think this covers things as well as I can manage at the moment.

As for improvements:

* I think that the `Settings` and `Trials` types in the trials could be unified into a single type in `GenGame`, eliminating the need for the `Model.Game` type.

* The functions in the trial modules are too similar to ignore the possibility of not having that separation. It might help to think in terms of a `Session` and a list of `Cards` or `Slides`, each of which is made of a view and a tiny piece of logic that can be composed together in arbitrary ways. You would then be able to make something like:

    [ (NoInput, FixationCross, Fixed 500)
    , (NoInput, CenterView NoBorder ( Url url ), Fixed 650)
    , (Indication Go, CenterView BlueBorder ( Url url ), TakesInt (\i -> 10000 - i))
    , (NoInput, RedCross, TakesSuccess (\b -> if b then 500 else 0))
    , (NoInput, FixationCross, Fixed 500)

    ...

    ]

This is a bit of pseudocode, the tuples would actually be union types and there would be some extra information for logging, but it is the direction I would want to take things.

That's about it, I think. Hopefully this helps you in navigating the code when you need to.