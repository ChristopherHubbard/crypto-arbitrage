-- Language extensions to the Haskell98 definition in GHC
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

    -- Imports necessary to run the handler
    import Foundation
    import Yesod

    -- Home route for the application -- Need Handler type from the foundation
    getHomeR :: Handler ()
    getHomeR = do
        sendFile "text/html" "../dist/index.html"