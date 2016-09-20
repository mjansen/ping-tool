{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Monad

import Data.Word
import Data.Char
import Data.Maybe
import qualified Data.ByteString.Char8 as BC
import qualified Data.Set as Set
import qualified Data.ByteString as B
import Data.Time
import Network.Socket hiding (sendTo, recvFrom)
import Network.Socket.ByteString (sendTo, recvFrom)
import Net.ICMP
import qualified Data.Array.IArray as A
import Net.PacketParsing -- (unparse, doUnparse)
import Net.Packet
import Net.IPv4
import Net.ICMP

import System.FilePath
import System.Directory

import GHC.Exts

ping :: Word16 -> Word16 -> B.ByteString -> IO ()
ping ident seqNr msg = do
  let address = SockAddrInet 0 0x0100007f
      content = pingPacket ident seqNr msg
  s <- socket AF_INET Raw 1
  ns <- mapM (sendTo s content) theWholeSubnetSs
  let fetch = do
        (response, sender) <- recvFrom s 1024
        let (r' :: Maybe (Net.IPv4.Packet Net.ICMP.Packet)) = doParse (toInPack . bsToChunk $ response)
        case r' of
          Nothing -> return ()
          Just r  -> let c = Net.IPv4.content r
                     in case c of
                       EchoReply (Echo ident' seqNr' _) -> print (sender, ident', seqNr')
                       _            -> return ()
        
  replicateM_ (length theWholeSubnetSs) fetch

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
           
main = ping 123 456 "hello there, this is me"

theWholeSubnetAs :: [HostAddress]
theWholeSubnetAs =
  map (\ i -> 0x0001a8c0 + i*256^3) [1 .. 254]

theWholeSubnetSs = map (SockAddrInet 0) theWholeSubnetAs
