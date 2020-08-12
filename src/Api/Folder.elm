module Api.Folder exposing (Folder (..))



type Folder
    = Folder
        { name       : String
        , photoUrls  : List String
        , subfolders : List Folder
        , expanded   : Bool
        }