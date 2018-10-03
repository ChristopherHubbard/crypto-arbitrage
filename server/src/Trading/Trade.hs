-- Language overloads
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE DeriveFoldable     #-}

module Trading.Trade
(
    Trade
) where

    import GHC.Generics
    import Trading.UserInfo
    import Trading.Currency
    import Trading.Order

    data DoubleEntry = DoubleEntry
        {
            fromUser :: User,
            toUser :: User,
            currency :: Currency,
            amount :: Amount
        } deriving (Eq, Show, Generic)

    data TradeGeneric a = TradeGeneric
        {
            from :: a,
            to :: a
        } deriving (Eq, Show, Functor, Traversable, Foldable, Generic)

    type Trade = TradeGeneric DoubleEntry