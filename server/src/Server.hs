-- Language extensions to the Haskell98 definition in GHC
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- Create this module -- Need to make a more modular structure for this server-side code
module Server where
    -- Import the Yesod framework
    import Yesod
    --import Yesod.Static -- Should this be used to set a default static directory

    -- Serve html, js, and css from this folder
    --staticFiles "../dist"

    -- App Data Type -- has no information in this example
    data App = App

    -- Create the routes using the mkYesod Template Haskell function and the parseRoutes quasi-quoter (important terms)
    mkYesod "App" [parseRoutes|
        / HomeR GET -- Route RouteName RequestType
        --/static StaticR Static getStatic -- This is supposed to be the static routes using yesod-static -- might want to look into this
        /build.js JSR GET
    |]

    -- Foundation Data type instance to start the application -- must be an instance of Yesod
    instance Yesod App

    -- Handler function for the build.js request
    getJSR :: Handler ()
    getJSR = do
        sendFile "text/javascript" "../dist/build.js"

    -- Handler function that returns the index html
    getHomeR :: Handler ()
    getHomeR = do
        sendFile "text/html" "../dist/index.html"

    -- Main starts the HelloWorld Routes on port 3000 using the Warp Development Server
    main :: IO ()
    main = do
        warp 3000 $ App