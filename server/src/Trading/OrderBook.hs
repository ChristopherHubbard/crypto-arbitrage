
module Trading.OrderBook 
(
    OrderBook
)where

    import qualified Data.Map as M
    import qualified Data.Sequence as Q
    import Trading.BidAsk
    import Trading.Order
    import Trading.Currency

    data OrderBookGeneric a = OrderBook
        {
            fromCurrency :: Currency,
            toCurrency :: Currency,
            bids :: M.Map Price (Q.Seq (TaggedBid a)),
            asks :: M.Map Price (Q.Seq (TaggedAsk a))
        } deriving (Eq, Show, Functor, Traversable, Foldable) -- Functor, Traversable, and Foldable??

    -- The orderbook is composed of the limit orders
    type OrderBook = OrderBookGeneric LimitOrder