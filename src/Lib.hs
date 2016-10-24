module Lib
     where

import qualified Data.Attoparsec.Text  as DAT
import qualified Data.Text             as T
import Data.Time
import Data.Time.Format
import Data.Time.Clock.POSIX

data Quote = Quote {  packetTime::UTCTime
                   , marketType::Int
                   , issueCode ::T.Text
                   , issueSeqNum::Int
                   , marketStatusType::Int
                   , totalBidVol::Int
                   , bestBidP1::Int
                   , bestBidQ1::Int
                   , bestBidP2::Int
                   , bestBidQ2::Int
                   , bestBidP3::Int
                   , bestBidQ3::Int
                   , bestBidP4::Int
                   , bestBidQ4::Int
                   , bestBidP5::Int
                   , bestBidQ5::Int
                   , totalAskVol::Int
                   , bestAskP1::Int
                   , bestAskQ1::Int
                   , bestAskP2::Int
                   , bestAskQ2::Int
                   , bestAskP3::Int
                   , bestAskQ3::Int
                   , bestAskP4::Int
                   , bestAskQ4::Int
                   , bestAskP5::Int
                   , bestAskQ5::Int
                   , numBestValidTotal::Int
                   , numBestAskQ1::Int
                   , numBestBidQ1::Int
                   , numBestBidQ2::Int
                   , numBestAskQ2::Int
                   , numBestBidQ3::Int
                   , numBestAskQ3::Int
                   , numBestBidQ4::Int
                   , numBestAskQ4::Int
                   , numBestBidQ5::Int
                   , numBestAskQ5::Int
                   , numBesAskValidTotal::Int
                   , quoteAcceptTime::UTCTime
                   } deriving Eq


parseTPacket::DAT.Parser Quote
parseTPacket = do
    packetTime       <- DAT.count 10 DAT.anyChar
    marketType       <- DAT.count 1 DAT.anyChar
    issueCode        <- DAT.count 12 DAT.anyChar
    issueSeqNum      <- DAT.count 3 DAT.anyChar
    marketStatusType <- DAT.count 2 DAT.anyChar
    totalBidVol      <- DAT.count 7 DAT.anyChar
    bestBidP1        <- DAT.count 5 DAT.anyChar
    bestBidQ1        <- DAT.count 7 DAT.anyChar
    bestBidP2        <- DAT.count 5 DAT.anyChar
    bestBidQ2        <- DAT.count 7 DAT.anyChar
    bestBidP3        <- DAT.count 5 DAT.anyChar
    bestBidQ3        <- DAT.count 7 DAT.anyChar
    bestBidP4        <- DAT.count 5 DAT.anyChar
    bestBidQ4        <- DAT.count 7 DAT.anyChar
    bestBidP5        <- DAT.count 5 DAT.anyChar
    bestBidQ5        <- DAT.count 7 DAT.anyChar
    totalAskVol      <- DAT.count 7 DAT.anyChar
    bestAskP1        <- DAT.count 5 DAT.anyChar
    bestAskQ1        <- DAT.count 7 DAT.anyChar
    bestAskP2        <- DAT.count 5 DAT.anyChar
    bestAskQ2        <- DAT.count 7 DAT.anyChar
    bestAskP3        <- DAT.count 5 DAT.anyChar
    bestAskQ3        <- DAT.count 7 DAT.anyChar
    bestAskP4        <- DAT.count 5 DAT.anyChar
    bestAskQ4        <- DAT.count 7 DAT.anyChar
    bestAskP5        <- DAT.count 5 DAT.anyChar
    bestAskQ5        <- DAT.count 7 DAT.anyChar
    numBestBidValidTotal       <- DAT.count 5 DAT.anyChar
    numBestBidQ1              <- DAT.count 4 DAT.anyChar
    numBestBidQ2              <- DAT.count 4 DAT.anyChar
    numBestBidQ3              <- DAT.count 4 DAT.anyChar
    numBestBidQ4              <- DAT.count 4 DAT.anyChar
    numBestBidQ5              <- DAT.count 4 DAT.anyChar
    numBesAskValidTotal       <- DAT.count 5 DAT.anyChar
    numBestAskQ1              <- DAT.count 4 DAT.anyChar
    numBestAskQ2              <- DAT.count 4 DAT.anyChar
    numBestAskQ3              <- DAT.count 4 DAT.anyChar
    numBestAskQ4              <- DAT.count 4 DAT.anyChar
    numBestAskQ5              <- DAT.count 4 DAT.anyChar
    quoteAcceptTime           <- DAT.count 8 DAT.anyChar

    return $ Quote      (posixSecondsToUTCTime $ fromIntegral (read (packetTime)::Int))
                        (read (marketType)::Int)
                        (T.pack issueCode)
                        (read (issueSeqNum)::Int)
                        (read (marketStatusType)::Int)
                        (read (totalBidVol)::Int)
                        (read (bestBidP1)::Int)
                        (read (bestBidQ1)::Int)
                        (read (bestBidP2)::Int)
                        (read (bestBidQ2)::Int)
                        (read (bestBidP3)::Int)
                        (read (bestBidQ3)::Int)
                        (read (bestBidP4)::Int)
                        (read (bestBidQ4)::Int)
                        (read (bestBidP5)::Int)
                        (read (bestBidQ5)::Int)
                        (read (totalAskVol)::Int)
                        (read (bestAskP1)::Int)
                        (read (bestAskQ1)::Int)
                        (read (bestAskP2)::Int)
                        (read (bestAskQ2)::Int)
                        (read (bestAskP3)::Int)
                        (read (bestAskQ3)::Int)
                        (read (bestAskP4)::Int)
                        (read (bestAskQ4)::Int)
                        (read (bestAskP5)::Int)
                        (read (bestAskQ5)::Int)
                        (read (numBestBidValidTotal)::Int)
                        (read (numBestAskQ1)::Int)
                        (read (numBestBidQ1)::Int)
                        (read (numBestBidQ2)::Int)
                        (read (numBestAskQ2)::Int)
                        (read (numBestBidQ3)::Int)
                        (read (numBestAskQ3)::Int)
                        (read (numBestBidQ4)::Int)
                        (read (numBestAskQ4)::Int)
                        (read (numBestBidQ5)::Int)
                        (read (numBestAskQ5)::Int)
                        (read (numBesAskValidTotal)::Int)
                        (parseTimeOrError True defaultTimeLocale "%k%M%S%Q" (reverse $ ( take 2 $ reverse $  quoteAcceptTime )++"."++(drop 2 $ reverse $  quoteAcceptTime)) :: UTCTime) -- inserts a "." so that time can be parsed to UTCTime


instance Show Quote where
  show (Quote pt mktType issueCode issueSeqNum mktStatType ttlVol
              bbp1 bbq1 bbp2 bbq2 bbp3 bbq3 bbp4 bbq4 bbp5 bbq5         -- bbp/q = best bid price/quantity
              ttlAskVol
              bap1 baq1 bap2 baq2 bap3 baq3 bap4 baq4 bap5 baq5         -- bap/q = best ask price/quantity
              numBestValidTotal _ _ _ _ _ _  _ _ _ _ _ acceptTime )
       =
       "packetTime: " ++  (show pt)
       ++ " AcceptTime: " ++ (show acceptTime)
       ++ " issueCode: " ++(T.unpack issueCode)
       ++" BIDS:"
       ++" "++(show bbq5)++"@"++(show bbp5)
       ++"  "++(show bbq4)++"@"++(show bbp4)
       ++"  "++(show bbq3)++"@"++(show bbp3)
       ++"  "++(show bbq2)++"@"++(show bbp2)
       ++"  "++(show bbq1)++"@"++(show bbp1)
       ++" ASKS:"
       ++" "++(show baq1)++"@"++(show bap1)
       ++"  "++(show baq2)++"@"++(show bap2)
       ++"  "++(show baq3)++"@"++(show bap3)
       ++"  "++(show baq4)++"@"++(show bap4)
       ++"  "++(show baq5)++"@"++(show bap5)
