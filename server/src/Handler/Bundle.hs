-- Language extensions to the Haskell98 definition in GHC
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Bundle where

    -- Import necessary modules
    import Foundation
    import Yesod

    -- Handler function for the build.js request
    getBundleR :: Handler ()
    getBundleR = do
        -- This has to have its own route since sendFile ends the handler
        sendFile "text/javascript" "../dist/build.js"