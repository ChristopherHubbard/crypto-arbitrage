
module Trading.Exchange
(
    Exchange
) where

    import Trading.OrderBook
    import Trading.Trade
    import Control.Concurrent.STM.TVar
    import qualified Data.Sequence as Q
    
    data Exchange = Exchange
        {
            orderBook :: TVar OrderBook, -- Book of orders waiting to be filled
            trades :: TVar (Q.Seq Trade) -- Previously executed trades
        }