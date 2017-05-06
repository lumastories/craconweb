module Mock exposing (..)

import Model exposing (MeStatement)


statements : List MeStatement
statements =
    [ { id = "123", essay = "I like food so much. It is so lovely, yes yes yes, oh boy! GIMME FOOD. I like to eat. hooray! This is my personal statement", public = True }
    , { id = "124", essay = "Motivation is so important, blablabl, I am loving this program, i like to eat but only healthy things, oh yeah!! woohoo!", public = False }
    , { id = "125", essay = "Motivation is so important, blablabl, I am loving this program, i like to eat but only healthy things, oh yeah!! woohoo!", public = False }
    , { id = "126", essay = "Motivation is so important, blablabl, I am loving this program, i like to eat but only healthy things, oh yeah!! woohoo!", public = True }
    , { id = "123", essay = "I like food so much. It is so lovely, yes yes yes, oh boy! GIMME FOOD. I like to eat. hooray! This is my personal statement", public = True }
    , { id = "123", essay = "I like food so much. It is so lovely, yes yes yes, oh boy! GIMME FOOD. I like to eat. hooray! This is my personal statement", public = True }
    , { id = "123", essay = "I like food so much. It is so lovely, yes yes yes, oh boy! GIMME FOOD. I like to eat. hooray! This is my personal statement", public = True }
    , { id = "123", essay = " another quote. This is so fun blablablaI like food so much. It is so lovely, yes yes yes, oh boy! GIMME FOOD. I like to eat. hooray! This is my personal statement", public = True }
    ]
