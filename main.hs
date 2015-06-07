{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment

import Data.Either
import Data.Bits
import Data.Char (intToDigit)
import Numeric (showHex, showIntAtBase)

import Vision.Image hiding (map)
import Vision.Image.Storage.DevIL (Autodetect (..), load, save)
import Vision.Primitive (ix2)
import Vision.Primitive.Shape

pixelGradient i j = if i < j then bit 0 :: Int else zeroBits :: Int

hashImg img =
  let
    Z :. height :. width = shape img
    indices = [i | i <- [0..height-1]]
    rowHashes = map (\row -> hashRow row img) indices
  in
    abs $ foldl (\prevHash j ->
                  let
                    (nextHash, _) = j
                  in
                   shift prevHash width .|. nextHash)
    (zeroBits :: Int) rowHashes

hashRow row img =
  let
    Z :. height :. width = shape img
    pixels = map (\x -> index img (Z :. row :. x)) [x | x <- [0..width-1]]
    hashPixels acc pixel = (shift hash 1 .|. pixelGradient prev next, pixel)
                           where (hash, GreyPixel prev) = acc
                                 GreyPixel next = pixel
  in
    foldl hashPixels (zeroBits :: Int, pixels !! 0) (drop 1 pixels)

hammingDistance x y =
  if length x == length y then
    foldl (+) 0 (zipWith (\i j -> if i == j then 0 else 1) x y)
  else
    (maxBound :: Int)

main = do
  args <- getArgs

  if length args /= 2 then
    error "Give two paths to images as arguments."
  else
    do
      img1 <- load Autodetect (args !! 0)
      img2 <- load Autodetect (args !! 1)

      case (img1,img2) of
       (Right (img1' :: RGB), Right (img2' :: RGB)) -> do

         let resized1 = resize NearestNeighbor (ix2 8 9) img1' :: RGB
         let resized2 = resize NearestNeighbor (ix2 8 9) img2' :: RGB

         let grey1 = convert resized1 :: Grey
         let grey2 = convert resized2 :: Grey

         let binaryHash1 = showIntAtBase 2 intToDigit (hashImg grey1) ""
         let binaryHash2 = showIntAtBase 2 intToDigit (hashImg grey2) ""

         putStrLn . show $ hammingDistance binaryHash1 binaryHash2
         
       otherwise -> putStrLn "Couldn\'t read both files."
