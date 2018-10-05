
module Trading.Exchange
(
    Exchange (..),
    newExchange
) where

    import Trading.OrderBook
    import Trading.Trade
    import Trading.Currency
    import Control.Concurrent.STM
    import Control.Concurrent.STM.TVar
    import qualified Data.Sequence as Q
    
    data Exchange = Exchange
        {
            orderBooks :: (Q.Seq OrderBook), -- Book of orders waiting to be filled -- one book for each pair
            trades :: (Q.Seq Trade) -- Previously executed trades
        }

    -- Create a new Exchange given a sequence of currency pairs
    newExchange :: (Q.Seq (Currency, Currency)) -> Exchange
    newExchange pairList =
        -- Map the elements to the exchange
        let books = fmap (\(pair) -> newOrderBook pair) pairList
            trades = Q.empty
        in Exchange
            {
                orderBooks = books,
                trades = trades
            }
