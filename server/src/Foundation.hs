-- Language extensions to the Haskell98 definition in GHC
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Foundation where

    import Yesod
    import Yesod.Static

    staticFiles "../dist/"

    -- Define the foundation type of the application here. Should include settings that activate on startup
    data App = App
        {
            getStatic :: Static
        }

    -- Create Yesod instance using this foundation type
    instance Yesod App

    -- Make the Yesod app using the config file -- better to consolidate the routes like this
    mkYesodData "App" $(parseRoutesFile "./src/config/routes")

    -- Create the static function
    -- staticGetter :: Static
    -- staticGetter = static "../dist"