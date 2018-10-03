-- Language overloads
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveFoldable             #-}

module Trading.OrderBook 
(
    OrderBook
) where

    import Data.Tagged
    import Data.Maybe
    import qualified Data.Map as M
    import qualified Data.Sequence as Q
    import Trading.BidAsk
    import Trading.Trade
    import Trading.Order
    import Trading.Currency

    type SeqMap k v = M.Map k (Q.Seq v)

    data OrderBookGeneric a = OrderBook
        {
            fromCurrency :: Currency,
            toCurrency :: Currency,
            bids :: SeqMap Price (TaggedBid a),
            asks :: SeqMap Price (TaggedAsk a)
        } deriving (Eq, Show, Functor, Traversable, Foldable) -- Functor, Traversable, and Foldable??

    -- The orderbook is composed of the limit orders
    type OrderBook = OrderBookGeneric LimitOrder

    -- Set up functions for the orderbook

    -- Fill a bid order -----------------------------------------------------------------------------------------------------------
    fillBid :: OrderBook -> (TaggedBid LimitOrder) -> ([Trade], OrderBook)
    fillBid orderBook bid =
        -- Match the bid and return based on remainder
        case matchBid orderBook bid of
            (Nothing, trades, book) -> (trades, book)
            (Just remainder, trades, book) -> (trades, addBid book remainder)

    -- Match the bid with the ask
    matchBid :: OrderBook -> (TaggedBid LimitOrder) -> (Maybe (TaggedBid LimitOrder), [Trade], OrderBook)
    matchBid orderBook bid =

    -- Trys to partially or fully execute the bid with appropriate ask
    executeBid :: (Currency, Currency) -> (TaggedBid LimitOrder) -> (TaggedAsk LimitOrder) -> (Maybe (TaggedBid LimitOrder), Maybe (TaggedAsk LimitOrder), Maybe Trade)
    executeBid (fromCurrency, toCurrency) bid ask =
        let 
        in

    -- Cancel a bid order -- Deletes it from the order book and updates the orderbook ---------------------------------------------
    cancelBid :: OrderBook -> (TaggedBid LimitOrder) -> OrderBook
    cancelBid orderBook@OrderBook {..} bid =
        let p = price $ unTagged bid
            remainingBids = deleteBid bids p bid
        in orderBook { bids = remainingBids }

    deleteBid :: (SeqMap Price (TaggedBid LimitOrder)) -> Price -> (TaggedBid LimitOrder) -> (SeqMap Price (TaggedBid LimitOrder))
    deleteBid bids p bid =
        fromMaybe (bids) $ do -- Get it from maybe on the do block -- default value is return this set of bids
            -- Get the sequence of bids for this price
            bidSeq <- (M.lookup p bids)
            -- Delete the price from the bids sequence
            let newMap = (M.delete p bids) -- Why does this need let?

            -- Find the index of the bid to delete in the seq
            case Q.findIndexL (== bid) bidSeq of
                Nothing -> Nothing -- If no index then return nothing
                Just i ->
                    -- Index of the bid was found -- delete it from the seq and then insert this new seq into the map
                    let newSeq = Q.deleteAt i bidSeq
                    in Just (M.insert p newSeq newMap)