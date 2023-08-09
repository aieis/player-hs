{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Rand (randomByteString)

import Graphics.Image hiding (on)
import Prelude as P
import System.Random (randomRIO)
import qualified GI.Gtk as Gtk
import GI.GdkPixbuf.Objects.Pixbuf
import GI.GdkPixbuf.Enums

import Foreign.Ptr
import Data.GI.Base

main :: IO ()
main = gtkwin

gtkwin :: IO ()
gtkwin = do
  Gtk.init Nothing
  win <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle win "Main"
  Gtk.onWidgetDestroy win Gtk.mainQuit
  #resize win 540 960

  bs <- randomByteString (200 * 200 * 3)
  print bs
  pb <- pixbufNewFromData bs  ColorspaceRgb False 8 200 200 (200 * 3) Nothing
  img <- Gtk.imageNewFromPixbuf (Just pb)

  box <- new Gtk.Box [#orientation := Gtk.OrientationVertical ]
  #add box img
  #add win box

  msg <- new Gtk.Label [ #label := ""]
  #packStart box msg True False 10

  btn <- new Gtk.Button [ #label := "Click me !" ]
  #packStart box btn False False 10

  on btn #clicked $ do
    bs <- randomByteString (200 * 200 * 3)
    pb <- pixbufNewFromData bs  ColorspaceRgb False 8 200 200 (200 * 3) Nothing
    Gtk.imageSetFromPixbuf img $ Just pb
  
  Gtk.widgetShowAll win
  Gtk.main

idxToRGB :: (Int, Int) -> Pixel RGB Double
idxToRGB (i, j) = PixelRGB (fromIntegral i) (fromIntegral j) (fromIntegral (i+j)) / 400

idxToRGBArr :: [Double] -> (Int, Int) -> Pixel RGB Double
idxToRGBArr arr = _idxToRGBArr
  where
    _idxToRGBArr :: (Int, Int) -> Pixel RGB Double
    _idxToRGBArr (i, j) = PixelRGB (arr!!idx) (arr!!(idx+1)) (arr!!(idx+2))
      where idx = i * 200 + j
  
      
randomList :: Int -> IO[Double]
randomList 0 = return []
randomList n = do
  r <- randomRIO (0,1)
  rs <- randomList (n-1)
  return (r:rs)

  
viewer :: IO()
viewer = do
  r <- randomList (200 * 200 * 3)
  let im = makeImageR VU (200, 200) (idxToRGBArr r)
    in writeImage "images/grad_grey.png" im
