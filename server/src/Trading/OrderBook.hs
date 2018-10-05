-- Language overloads
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveFoldable             #-}

module Trading.OrderBook where

    import Data.Tagged
    import Data.Maybe
    import qualified Data.Map as M
    import qualified Data.Sequence as Q
    import Trading.BidAsk
    import Trading.Order
    import Trading.Currency
    import Trading.Trade

    type SeqMap k v = M.Map k (Q.Seq v)

    data OrderBookF a = OrderBook
        {
            fromCurrency :: Currency,
            toCurrency :: Currency,
            bids :: SeqMap Price (TaggedBid a),
            asks :: SeqMap Price (TaggedAsk a)
        } deriving (Eq, Show, Functor, Traversable, Foldable) -- Functor, Traversable, and Foldable??

    type OrderBook = OrderBookF LimitOrder

    -- Set up functions for the orderbook

    -- Fill a bid order -----------------------------------------------------------------------------------------------------------
    fillBid :: OrderBook -> (TaggedBid LimitOrder) -> ([Trade], OrderBook)
    fillBid orderBook bid =
        -- Match the bid and return based on remainder
        case matchBid orderBook bid of
            -- Trade went through with no remainder -- return the book and trades
            (Nothing, trades, book) -> (trades, book)
            -- Add the remaining part of the bid back into the book
            (Just remainder, trades, book) -> (trades, addBid book remainder)

    -- Match the bid with the ask
    matchBid :: OrderBook -> (TaggedBid LimitOrder) -> (Maybe (TaggedBid LimitOrder), [Trade], OrderBook)
    matchBid orderBook@OrderBook {..} bid =
        -- Loop through and keep matching the bid until it can't be matched or is filled completely
        let loop :: ((TaggedBid LimitOrder), [Trade], OrderBook) -> (Maybe (TaggedBid LimitOrder), [Trade], OrderBook)
            loop x@(bid, trades, book) =
                -- Find the lowest ask in the book to try to fill
                case minimumAsk book of
                    -- Order book has no asks to fill
                    (Nothing, _) -> (Just bid, trades, book)
                    -- Order book has an ask to be filled
                    (Just minAsk, bookWithoutAsk) ->
                        case executeBid (fromCurrency, toCurrency) bid minAsk of
                            -- Bid was not matched with the ask
                            (Just bid, Just _, Nothing) -> (Just bid, trades, orderBook)
                            -- Bid was matched partially
                            (Just remainder, Nothing, Just trade) ->
                                loop (remainder, trade:trades, bookWithoutAsk)
                            -- Ask was matched partially
                            (Nothing, Just remainder, Just trade) -> (Nothing, trade:trades, addAsk bookWithoutAsk remainder)
                            -- Exact match with bid and ask
                            (Nothing, Nothing, Just trade) -> (Nothing, trade:trades, bookWithoutAsk)
        in loop (bid, [], orderBook)

    -- Trys to partially or fully execute the bid with appropriate ask
    executeBid :: (Currency, Currency) -> (TaggedBid LimitOrder) -> (TaggedAsk LimitOrder) -> (Maybe (TaggedBid LimitOrder), Maybe (TaggedAsk LimitOrder), Maybe Trade)
    executeBid (fromCurrency, toCurrency) bid ask =
        -- Create a trade between these currencies
        let 
            -- This is the bid order -- tag removed
            bidOrder = unTagged bid
            askOrder = unTagged ask

            -- Get the users involved
            bidUser = user bidOrder
            askUser = user askOrder

            -- Extract the amounts
            bidFrom = fromAmount bidOrder
            bidTo = toAmount bidOrder
            askFrom = fromAmount askOrder
            askTo = toAmount askOrder

            -- Bid price and asking price -- amount of currency 1 to currency 2 -- Bid cant be higher, ask cant be lower
            bidPrice = floor (bidFrom / bidTo)
            askPrice = ceiling (askFrom / askTo)

            -- Create the entries
            fromEntry = DoubleEntry
                {
                    fromUser = askUser,
                    toUser = bidUser,
                    currency = fromCurrency,
                    amount = askFrom --  might need to be updated
                }

            toEntry = DoubleEntry
                {
                    fromUser = bidUser,
                    toUser = askUser,
                    currency = toCurrency,
                    amount = bidTo -- might need to be updated
                }

            -- Create the trade with the double entries
            newTrade = Trade
                {
                    from = fromEntry,
                    to = toEntry
                }
            
            -- Create newBids and newAsk if necessary
            (newBid, newAsk) = case bidTo `compare` askFrom of
                -- If the bid amount was less than the ask -- create new ask for leftover
                LT ->
                    let newAsk = Tagged $ LimitOrder
                            {
                                user = askUser,
                                fromAmount = askFrom - bidTo, -- Amount left to sell
                                toAmount = askTo - bidFrom -- Amount left to buy
                            }
                    in (Nothing, Just newAsk)
                -- The bid wasnt fully satisfied by this ask -- create new bid
                GT ->
                    let newBid = Tagged $ LimitOrder
                            {
                                user = bidUser,
                                fromAmount = bidFrom - askTo, -- Amount left to sell
                                toAmount = bidTo - askFrom -- Amount left to buy
                            }
                    in (Just newBid, Nothing)
                -- The bid and ask cancelled each other perfectly
                EQ -> (Nothing, Nothing)
        in
            if bidPrice >= askPrice
            -- The bid is greater than the ask -- trade was made
            then (newBid, newAsk, Just newTrade)
            -- The bid was less than the ask -- no deal!
            else (Just bid, Just ask, Nothing)

    -- Fill a ask order -----------------------------------------------------------------------------------------------------------
    fillAsk :: OrderBook -> (TaggedAsk LimitOrder) -> ([Trade], OrderBook)
    fillAsk orderBook ask =
        -- Match the ask and return based on remainder
        case matchAsk orderBook ask of
            -- Trade went through with no remainder -- return the book and trades
            (Nothing, trades, book) -> (trades, book)
            -- Add the remaining part of the ask back into the book
            (Just remainder, trades, book) -> (trades, addAsk book remainder)

    -- Match the bid with the ask
    matchAsk :: OrderBook -> (TaggedAsk LimitOrder) -> (Maybe (TaggedAsk LimitOrder), [Trade], OrderBook)
    matchAsk orderBook@OrderBook {..} ask =
        -- Loop through and keep matching the ask until it can't be matched or is filled completely
        let loop :: ((TaggedAsk LimitOrder), [Trade], OrderBook) -> (Maybe (TaggedAsk LimitOrder), [Trade], OrderBook)
            loop x@(ask, trades, book) =
                -- Find the lowest ask in the book to try to fill
                case maximumBid book of
                    -- Order book has no asks to fill
                    (Nothing, _) -> (Just ask, trades, book)
                    -- Order book has an ask to be filled
                    (Just maxBid, bookWithoutBid) ->
                        case executeBid (fromCurrency, toCurrency) maxBid ask of
                            -- Ask was not matched with the bid
                            (Just _, Just _, Nothing) -> (Just ask, trades, orderBook)
                            -- Ask was matched partially
                            (Nothing, Just remainder, Just trade) ->
                                loop (remainder, trade:trades, bookWithoutBid)
                            -- Ask was matched partially
                            (Just remainder, Nothing, Just trade) -> (Nothing, trade:trades, addBid bookWithoutBid remainder)
                            -- Exact match with bid and ask
                            (Nothing, Nothing, Just trade) -> (Nothing, trade:trades, bookWithoutBid)
        in loop (ask, [], orderBook)

    -- Add a bid to the orderbook
    addBid :: OrderBook -> (TaggedBid LimitOrder) -> OrderBook
    addBid orderBook@OrderBook {..} bid =
        -- Get the price and add the bid to the sequence in the map
        let p = price $ unTagged bid
            seq = case M.lookup p bids of
                (Nothing) -> Q.empty
                (Just seq) -> seq
            -- Insert the bid into the sequence
            newBids = seq Q.|> bid
        -- Add the new sequence to the map and set the bids with it
        in orderBook { bids = M.insert p newBids bids }

    -- Add an ask to the orderbook
    addAsk :: OrderBook -> (TaggedAsk LimitOrder) -> OrderBook
    addAsk orderBook@OrderBook {..} ask =
        -- Get the price and add the ask to the sequence in the map
        let p = price $ unTagged ask
            seq = case M.lookup p asks of
                (Nothing) -> Q.empty
                (Just seq) -> seq
            -- Insert the ask into the sequence
            newAsks = seq Q.|> ask
        -- Add the new sequence to the map
        in orderBook { asks = M.insert p newAsks asks }
    
    -- Cancel a bid or ask order -- Deletes it from the order book and updates the orderbook ---------------------------------------------
    cancelBid :: OrderBook -> (TaggedBid LimitOrder) -> OrderBook
    cancelBid orderBook@OrderBook {..} bid =
        let p = price $ unTagged bid
            remainingBids = deleteBid bids p bid
        in orderBook { bids = remainingBids }

    cancelAsk :: OrderBook -> (TaggedAsk LimitOrder) -> OrderBook
    cancelAsk orderBook@OrderBook {..} ask =
        let p = price $ unTagged ask
            remainingAsks = deleteAsk asks p ask
        in orderBook { asks = remainingAsks }

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

    deleteAsk :: (SeqMap Price (TaggedAsk LimitOrder)) -> Price -> (TaggedAsk LimitOrder) -> (SeqMap Price (TaggedAsk LimitOrder))
    deleteAsk asks p ask =
        fromMaybe (asks) $ do -- Get it from maybe on the do block -- default value is return this set of bids
            -- Get the sequence of asks for this price
            askSeq <- (M.lookup p asks)
            -- Delete the price from the asks sequence
            let newMap = (M.delete p asks) -- Why does this need let?

            -- Find the index of the ask to delete in the seq
            case Q.findIndexL (== ask) askSeq of
                Nothing -> Nothing -- If no index then return nothing
                Just i ->
                    -- Index of the ask was found -- delete it from the seq and then insert this new seq into the map
                    let newSeq = Q.deleteAt i askSeq
                    in Just (M.insert p newSeq newMap)
        
    -- Functions to get the min ask and highest bid from the orderbook
    minimumAsk :: OrderBook -> (Maybe (TaggedAsk LimitOrder), OrderBook)
    minimumAsk orderBook@OrderBook {..} =
        -- Get the min price sequence from the asks -- removes key pair from map
        let (minAsk, book) = case (M.minViewWithKey asks) of
                (Nothing) -> (Nothing, orderBook) -- What price returns in this case?
                (Just ((p, askSeq), askWithoutKey)) ->
                    case Q.lookup 0 askSeq of
                        (Nothing) -> (Nothing, orderBook)
                        (Just ask) -> 
                            let
                                -- Remove the ask taken from the ask sequence for this price
                                newSeq = Q.deleteAt 0 askSeq
                                -- Insert the new sequence at the price into the map
                                newAsks = M.insert p newSeq askWithoutKey
                                -- Create the new book using the newAsks -- shouldnt have the 
                                newBook = orderBook { asks = newAsks }
                            in (Just ask, newBook)
        -- Return the minAsk and the newBook without that ask present
        in (minAsk, book)

    maximumBid :: OrderBook -> (Maybe (TaggedBid LimitOrder), OrderBook)
    maximumBid orderBook@OrderBook {..} =
        -- Get the min price sequence from the bids -- removes key pair from map
        let (maxBid, book) = case (M.maxViewWithKey bids) of
                (Nothing) -> (Nothing, orderBook) -- What price returns in this case?
                (Just ((p, bidSeq), bidWithoutKey)) ->
                    -- Get the first bid at this price -- amounts might be different but price is all the same
                    case Q.lookup 0 bidSeq of
                        (Nothing) -> (Nothing, orderBook)
                        (Just bid) -> 
                            let
                                -- Remove the bid taken from the ask sequence for this price
                                newSeq = Q.deleteAt 0 bidSeq
                                -- Insert the new sequence at the price into the map
                                newBids = M.insert p newSeq bidWithoutKey
                                -- Create the new book using the newAsks -- shouldnt have the 
                                newBook = orderBook { bids = newBids }
                            in (Just bid, newBook)
        -- Return the minAsk and the newBook without that ask present
        in (maxBid, book)