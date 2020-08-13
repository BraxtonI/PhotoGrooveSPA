module Api.Photo exposing (Photo)


type alias Photo =
    { title       : String
    , url         : String
    , size        : Int
    , relatedUrls : List String
    }