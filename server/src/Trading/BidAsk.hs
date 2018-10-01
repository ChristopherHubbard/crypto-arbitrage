
module Trading.BidAsk
(
    TaggedBid,
    TaggedAsk
) where

    -- Imports
    import Data.Tagged

    -- Define the bid ask data types
    data PhantomBid
    data PhantomAsk

    type TaggedBid a = Tagged PhantomBid a
    type TaggedAsk a = Tagged PhantomAsk a
