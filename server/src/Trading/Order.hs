-- Language overloads
{-# LANGUAGE DuplicateRecordFields      #-}

module Trading.Order
(
    MarketOrder,
    LimitOrder,
    Amount,
    Price
) where

    -- Imports
    import GHC.Generics
    import Trading.UserInfo

    -- Use alias for Double for the amounts?
    type Amount = Double
    type Price = Double

    -- Set up the Market and Limit order TypeClasses

    -- Market orders to be executed when the 
    data MarketOrder = MarketOrder
        {
            order_user :: User,
            order_amount :: Amount
        } deriving (Eq, Show) -- Why use Generic??

    data LimitOrder = LimitOrder
        {
            order_user :: User,
            order_fromAmount :: Amount,
            order_toAmount :: Amount
        } deriving (Eq, Show) -- Why use Generic