{-# LANGUAGE OverloadedStrings,
             ScopedTypeVariables #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar

import Data.Word
import Data.Char
import Data.Maybe
import Data.List (intercalate, nub)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Set as Set
import qualified Data.ByteString as B
import Data.Time
import qualified Data.Map.Strict as Map

import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString (sendTo, recvFrom)
import Net.ICMP
import qualified Data.Array.IArray as A
import Net.PacketParsing
import Net.Packet
import Net.IPv4
import Net.ICMP

import Text.Printf

import System.Environment
import System.FilePath
import System.Directory
import System.Process

import GHC.Exts

--------------------------------------------------------------------------------

ping :: Socket -> [SockAddr] -> Word16 -> Word16 -> B.ByteString -> IO ()
ping s addrs ident seqNr msg = do
  let address = SockAddrInet 0 0x0100007f
      content = pingPacket ident seqNr msg
  ns <- mapM (sendTo s content) addrs
  let fetch = do
        (response, sender) <- recvFrom s 1024
        now <- getCurrentTime
        let (r' :: Maybe (Net.IPv4.Packet Net.ICMP.Packet)) = doParse (toInPack . bsToChunk $ response)
        case r' of
          Nothing -> return ()
          Just r  -> let c = Net.IPv4.content r
                     in case c of
                       EchoReply (Echo ident' seqNr' msg) ->
                         let delta = case parseTimeM False defaultTimeLocale "%Y-%m-%d %T%Q UTC" (BC.unpack . chunkToBS $ msg) of
                               Nothing -> 0
                               Just start -> diffUTCTime now start
                         in print (sender, ident', seqNr', {- chunkToBS msg, now, -} delta)
                       _            -> return ()
        
  replicateM_ (length addrs) fetch

pingSend :: Socket -> [SockAddr] -> Word16 -> Word16 -> B.ByteString -> IO ()
pingSend s addrs ident seqNr msg = do
  let address = SockAddrInet 0 0x0100007f
      content = pingPacket ident seqNr msg
  ns <- mapM (sendTo s content) addrs
  -- print ns
  return ()

pingCollect :: Socket -> MVar (Map.Map SockAddr [EchoMsg]) -> IO ()
pingCollect s db = do
  (response, sender) <- recvFrom s 1024
  now <- getCurrentTime
  let (r' :: Maybe (Net.IPv4.Packet Net.ICMP.Packet)) = doParse (toInPack . bsToChunk $ response)
  case r' of
   Nothing -> return ()
   Just r  -> let c = Net.IPv4.content r
              in case c of
                  EchoReply cc@(Echo ident' seqNr' msg) ->
                    let delta = case parseTimeM False defaultTimeLocale "%Y-%m-%d %T%Q UTC" (BC.unpack . chunkToBS $ msg) of
                          Nothing -> 0
                          Just start -> diffUTCTime now start
                    in do -- print (sender, ident', seqNr', {- chunkToBS msg, now, -} delta)
                          db' <- takeMVar db
                          let cs = maybe [] id $ Map.lookup sender db'
                          putMVar db (Map.insert sender (cc:cs) db')
                  _ -> return ()
  pingCollect s db

--------------------------------------------------------------------------------

bsToChunk :: B.ByteString -> Chunk
bsToChunk str = A.listArray (0, B.length str - 1) . B.unpack $ str

chunkToBS :: Chunk -> B.ByteString
chunkToBS c = B.pack . A.elems $ c

pingPacket :: Word16 -> Word16 -> B.ByteString -> B.ByteString
pingPacket idNr seqNr content =
  let payload = bsToChunk content
  in B.concat . map (B.pack . elems)
              . chunks
              . doUnparse
              $ EchoRequest (Echo idNr seqNr payload)

--------------------------------------------------------------------------------
              
theWholeSubnetAs :: (Word8, Word8, Word8, Word8) -> Int -> [HostAddress]
theWholeSubnetAs (a8, b8, c8, d8) nbits =
  let combine :: (Word32, Word32, Word32, Word32) -> Word32
      combine (a, b, c, d) = d + 256*(c + 256*(b + 256*a))
      split :: Word32 -> (Word32, Word32, Word32, Word32)
      split x = (a, b, c, d)
        where (a, ar) = x  `quotRem` (256^3)
              (b, br) = ar `quotRem` (256^2)
              (c, d ) = br `quotRem` (256  )
      swap :: Word32 -> Word32
      swap x = let (a, b, c, d) = split x in combine (d, c, b, a)
      addr   = (combine ( fromIntegral a8
                        , fromIntegral b8
                        , fromIntegral c8
                        , fromIntegral d8 ) `quot` hRange) * hRange
      hRange = 2^(32 - nbits)
      nRange = 2^nbits
      nMask  = (nRange - 1) * hRange
  in map (\ i -> swap (addr + i)) [1 .. hRange - 2]

theWholeSubnetSs addr nbits = map (SockAddrInet 0) $ theWholeSubnetAs addr nbits

parseSubnet :: String -> [SockAddr]
parseSubnet str =
  let (addrStr, ('/':maskLenStr)) = break (== '/') str
      [a, b, c, d] = fmap read . words . map (\ c -> if c == '.' then ' ' else c) $ addrStr
  in theWholeSubnetSs (a, b, c, d) (read maskLenStr)

--------------------------------------------------------------------------------

pingTimeStamp :: Socket -> [SockAddr] -> Word16 -> Word16 -> IO ()
pingTimeStamp s addrs ident seqNr = do
  now <- getCurrentTime
  ping s addrs ident seqNr (BC.pack . show $ now)

pingTimeStampSend :: Socket -> [SockAddr] -> Word16 -> Word16 -> IO ()
pingTimeStampSend s addrs ident seqNr = do
  now <- getCurrentTime
  pingSend s addrs ident seqNr (BC.pack . show $ now)

--------------------------------------------------------------------------------

showDB :: Map.Map SockAddr [EchoMsg] -> IO ()
showDB db = do
  [rows :: Int, cols :: Int] <- map read . words <$> readProcess "stty" [ "-F", "/dev/tty", "size" ] ""
  let xs           = sortWith fst . Map.toList . Map.filter (not . null) $ db
      as           = map (takeWhile (/= ':') . show . fst) xs
      prefixLen    = length . commonPrefix $ as
      aWidth       = maximum $ 0:map length as
      items        = map (showEntry aWidth prefixLen) xs
      maxWidth     = maximum (0:map length items)
      separator    = (" | " :: String)
      itemsPerLine = max 1 ((cols + length separator) `quot` (maxWidth + length separator))
      itemss       = groupN itemsPerLine items
      itemsss      = map (intercalate separator) itemss
  -- print itemsPerLine
  putStr ("\ESC[H" ++ unlines itemsss)

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs =
  let (xs0, rest) = splitAt n xs
  in xs0 : groupN n rest

commonPrefix :: Eq a => [[a]] -> [a]
commonPrefix xs | xs == [] = []
                | any null xs = []
                | (length . nub . map head $ xs) > 1 = []
                | otherwise = head (head xs) : commonPrefix (map tail xs)

showEntry :: Int -> Int -> (SockAddr, [EchoMsg]) -> String
showEntry w pl (sa, es) = printf "%s %4d" (take (w - pl) $ address ++ repeat ' ') (length es)
  where
    address = takeWhile (/= ':') . drop pl . show $ sa

--------------------------------------------------------------------------------

main :: IO ()
main = do
  [subnetStr, countStr] <- getArgs
  let subnet = parseSubnet subnetStr
      count = read countStr
  s <- socket AF_INET Raw 1
  db <- newMVar . Map.fromList . map (\ a -> (a, [])) $ subnet
  putStr "\ESC[2J"
  forkIO $ pingCollect s db
  let { swarm seqNr = do
           db' <- readMVar db
           let hasRecent = not . null
           let subnet = if seqNr `rem` 60 == 0
                        then Map.keys db'
                        else Map.keys . Map.filter hasRecent $ db'
           pingTimeStampSend s subnet 123 seqNr
           forkIO (showDB db')
           threadDelay 1000000 }
  mapM swarm [0 .. count - 1]
  threadDelay 5000000
