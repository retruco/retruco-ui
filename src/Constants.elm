module Constants exposing (..)

-- KEYS


debateKeyIds : List String
debateKeyIds =
    [ "con", "option", "pro", "remark", "source" ]


duplicateOfKeyId : String
duplicateOfKeyId =
    "duplicate-of"


imageLogoPathKeyIds : List String
imageLogoPathKeyIds =
    [ "logo" ]


imageScreenshotPathKeyIds : List String
imageScreenshotPathKeyIds =
    [ "screenshot" ]


imagePathKeyIds : List String
imagePathKeyIds =
    imageLogoPathKeyIds ++ imageScreenshotPathKeyIds


nameKeyIds : List String
nameKeyIds =
    [ "name" ]
