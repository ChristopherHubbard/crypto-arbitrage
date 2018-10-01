-- Language extensions to the Haskell98 definition in GHC
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- Create this module -- Need to make a more modular structure for this server-side code
module Main where
    -- Import the Yesod framework
    import Yesod
    import Yesod.Static -- Should this be used to set a default static directory
    import Handler.Home
    import Handler.Bundle
    import Foundation

    mkYesodDispatch "App" resourcesApp

    -- Main starts the HelloWorld Routes on port 3000 using the Warp Development Server
    main :: IO ()
    main = do
        -- Can I extract this to a more central location for this static folder? Does static even help me??
        static@(Static settings) <- static "../dist/"
        warp 3000 $ (App { getStatic = static })