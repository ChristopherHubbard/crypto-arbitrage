{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DeriveFoldable     #-}

module Trading.UserInfo 
(
    Address,
    User (..)
) where

    import GHC.Generics
    import Data.Text
    import qualified Data.Sequence as Q

    -- Address type alias
    type Address = Text

    -- User data type -- store an sequence of addresses for the different wallets
    data User = User
        {
            username :: Text,
            password :: Maybe Text, -- Should this be stored like this??
            phone :: Maybe Text,
            email :: Maybe Text,
            addresses :: Q.Seq Address -- List of crypto addresses -- User should set these in the UI (no internal wallet!)
        } deriving (Eq, Show)