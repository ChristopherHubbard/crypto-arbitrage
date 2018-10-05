-- Language overloads
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveFoldable             #-}

module Trading.Order
(
    MarketOrder (..),
    LimitOrder (..),
    Amount,
    Price,
    price
) where

    -- Imports
    import GHC.Generics
    import Trading.UserInfo

    -- Use alias for Double for the amounts?
    type Amount = Double
    type Price = Double

    -- Set up the Market and Limit order TypeClasses

    -- Market orders to be executed when the user wants current price
    data MarketOrder = MarketOrder
        {
            mUser :: User,
            mAmount :: Amount
        } deriving (Eq, Show, Generic) -- Why use Generic??

    -- Limit orders to be executed when the price hits price
    data LimitOrder = LimitOrder
        {
            user :: User,
            fromAmount :: Amount, -- Amount of base currency
            toAmount :: Amount -- Amount of target currency
        } deriving (Eq, Show, Generic) -- Why use Generic

    -- Function to find the price of an order between arbitrary currencies
    price :: LimitOrder -> Price
    price LimitOrder {..} = -- Deconstruct the limit order's elements
        fromAmount / toAmount