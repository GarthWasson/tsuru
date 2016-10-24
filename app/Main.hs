module Main where

import Lib
import Network.Pcap
import Data.Conduit
import Control.Applicative
import qualified Data.Conduit.Binary   as CB
import qualified Data.Conduit.List     as CL
import qualified Data.Attoparsec.Text  as DAT
import qualified Data.Text             as T
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import Data.Conduit.Attoparsec
import qualified Options               as O
import System.IO
import Control.Monad.IO.Class
import Control.Monad
import Control.Monad.Trans.Resource
import qualified Network.Pcap.Conduit  as NPC
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time
import Data.Time.Format
import Debug.Trace
import Data.Maybe
import qualified Data.List as DL

debug :: c -> String -> c
debug = flip trace

data MainOptions = MainOptions
    { optFilepath:: String
    , optSort::Bool
    }

instance O.Options MainOptions where
    defineOptions = pure MainOptions
        <*> O.simpleOption "filepath" ""
            "Path to pcap file"
        <*> O.simpleOption "r" False
            "Whether or not to sort by quote acceptTime"

main = O.runCommand $ \opts args ->do
    let path = (optFilepath opts)
    f <- openOffline path
    let pcapSource = NPC.sourceOffline path
    if optSort opts
      then
        runResourceT
        $ pcapSource
        $$ isTypeQuote
        =$ CL.filter (\x -> BC.length (snd x) > 160)
        =$ CL.map (\x ->(fst x,T.pack $ BC.unpack (snd x)))
        =$ CL.map (\x -> (fst x, (head . tail $ ( T.splitOn (T.pack "B603")) (snd x)) ))
        =$ CL.map (\x -> (fst x, head $ T.splitOn (T.pack "\255") (snd x)) )
        =$ CL.map (\x ->  T.append (T.pack $ take 10 $ drop 21 ( show (fst x)) ) (snd x) )
        =$ conduitParser (parseTPacket)
        =$ reorderByAcceptTime []
        =$ CL.map extractInfoToPrint
        =$ printPacket

      else
        runResourceT
        $ pcapSource
        $$ isTypeQuote
        =$ CL.filter (\x -> BC.length (snd x) > 160)
        =$ CL.map (\x ->(fst x,T.pack $ BC.unpack (snd x)))
        =$ CL.map (\x -> (fst x, (head . tail $ ( T.splitOn (T.pack "B603")) (snd x)) ))
        =$ CL.map (\x -> (fst x, head $ T.splitOn (T.pack "\255") (snd x)) )
        =$ CL.map (\x ->  T.append (T.pack $ take 10 $ drop 21 ( show (fst x)) ) (snd x) )
        =$ conduitParser (parseTPacket)
        =$ CL.map extractInfoToPrint
        =$ printPacket

-- ---------
-- Functions
-- ---------

reorderByAcceptTime :: [(PositionRange,Quote)] ->  Conduit  (PositionRange,Quote) (ResourceT IO) (PositionRange,Quote)
-- await multiple and then sort when list is large enough or upstream terminates
reorderByAcceptTime sortedList
  | (length sortedList) < 1 =
    do
      list3Sec <- getNext3Seconds  [Just a | a<-sortedList]
      let sorted = DL.sortBy acceptTimeCompare $ catMaybes list3Sec
      case (head list3Sec) of
          Nothing -> return ()                                     --terminates conduit
          Just a  -> do reorderByAcceptTime sorted
  | otherwise =
    do
      list3Sec <- getNext3Seconds [Just a|a<-sortedList]
      if (isNothing (last list3Sec))                               --true if last element added was Nothing
        then do
                mapM yield sortedList
                reorderByAcceptTime []
        else do
               let listToInsert  = catMaybes list3Sec
               let newSortedList = DL.sortBy acceptTimeCompare $ listToInsert
               yield $ newSortedList!!0
               reorderByAcceptTime (tail newSortedList)

getNext3Seconds []  = do
             a<-await
             case a of
               Nothing -> return [Nothing] -- []
               Just x  -> getNext3Seconds $ (Just x):[]
getNext3Seconds list= do
             a <- await
             case a of
               Nothing -> return $ list++[Nothing]
               Just x  -> do
                 let timeOld = quoteAcceptTime (snd . fromJust $ list!!0)
                 let timeNew = quoteAcceptTime (snd $ fromJust a)
                 if ( (utctDayTime timeNew) > (secondsToDiffTime 3) + (utctDayTime timeOld) )
                   then return $  list++[a]
                 else  getNext3Seconds (list++[a])

acceptTimeCompare::(PositionRange,Quote)->(PositionRange,Quote)->Ordering
acceptTimeCompare a b
               | (quoteAcceptTime $ snd a) > (quoteAcceptTime $ snd b) = GT
               |  otherwise                                            = LT

isTypeQuote :: Conduit  (PktHdr,BC.ByteString) (ResourceT IO) (PktHdr,BC.ByteString)
isTypeQuote = CL.map (processPossibleQuote)
              where
                processPossibleQuote input
                   | BC.isInfixOf (BC.pack "B603") (snd input) = input
                   | otherwise = (fst input,BC.pack "not a quote")

printPacket = CL.mapM_ (liftIO . print)

extractInfoToPrint (posRange,quote)= show quote
