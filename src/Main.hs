module Main where
-- import Graphics.Imlib
import System.Environment
import System.Exit
import Control.Applicative
import Codec.Picture
import Codec.Picture.Metadata as M
import Data.Either
import Data.Maybe
import Data.List.Split
-- import qualified Data.ByteString.Lazy as B
usage :: IO ()
usage =
  do
    progName <- getProgName
    putStrLn $ unwords ["usage:", progName, "<input1>","<input2>","<bar-width>", "<output>"]
    putStrLn "\tinput1, input2: Input images with matching dimensions"
    putStrLn "\tbar-width: Width of the segments taken from each input image"
    putStrLn "\toutput: Newly generated image with twice the input images' size"
    
main :: IO ()
main =
  do
    args <- getArgs
    if length args < 4 then
      (do
        usage
        exitFailure
      )
    else
     interweave (args !! 0) (args !! 1) (read $ args !! 2) (args !! 3)

interweave :: String -> String -> Int -> String -> IO ()
interweave in1 in2 segmentWidth out =
  do
    -- Try to load images
--    image1 <- loadImage in1
--    image2 <- loadImage in2
    input1 <- readImageWithMetadata in1
    input2 <- readImageWithMetadata in2
    if isLeft input1 then
      die $ "Failed to load image " ++ in1
    else if isLeft input2 then
      die $ "Failed to load image " ++ in2
    else
    -- Check matching dimensions
--    contextSetImage image1
--    (x1, y1) <- liftA2 (,) imageGetWidth imageGetHeight
--    contextSetImage image2
--    (x2, y2) <- liftA2 (,) imageGetWidth imageGetHeight
      do
        let Right (image1,metadata1) = input1
        let Right (image2,metadata2) = input2
        let [x1,y1,x2,y2] = map (fromIntegral . fromJust) [M.lookup M.Width metadata1, M.lookup M.Height metadata1, M.lookup M.Width metadata2, M.lookup M.Height metadata2]
        let lookups = [convertRGB16 image1, convertRGB16 image2]
        let lookupMap = createLookupMap 2 segmentWidth x1
        if x1 /= x2 || y1 /= y2 then
          die $ "Input dimensions don't match: x1=" ++ show x1 ++ " x2=" ++ show x2 ++ " y1=" ++ show y1 ++ " y2=" ++ show y2
        else
          saveJpgImage 100 out $ ImageRGB16 $ generateImage (\x y -> let (img,pos) = lookupMap !! x in pixelAt (lookups !! (img -1)) (pos-1) y) --if x < 40 then pixelAt (lookups !! 0) x y else PixelRGB16 0 0 0)
          (x1 * 2) y1

createLookupMap :: Int -> Int -> Int -> [(Int,Int)]
createLookupMap nrImages segmentWidth imageWidth =
  let
    -- first component is the number of the image to look up pixel
    imgs = cycle $ concat [replicate segmentWidth x | x <- [1..nrImages]]
    -- second component is the index of the pixel in a line
    pxls = concat $ concatMap (replicate nrImages) $ chunksOf segmentWidth [1..imageWidth]
  in
    zip imgs pxls
  -- [(if even m then 1 else 2,m) | x <- [1..complete*2], let m = x `mod` (segment + 1)]
  
