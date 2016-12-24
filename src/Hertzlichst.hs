module Hertzlichst where

import Temporal.Music
import Temporal.Music.Demo
import Temporal.Music.Western.P12
import Temporal.Music.Demo.GeneralMidi
import System.Cmd(system)

voice p1 p2 = 
  setScale (pyth 216.0)
  $ mel (notes ++ (p1 : (notes ++ [ p2 ])))
  where
    notes = [ en n0, qn $ loud n0, en n0, qn n0, hn $ loud n0, qn n0 ]

shiftPattern :: (Integral a) => [a] -> [Score b] -> [[Score b]]
shiftPattern p vs =
  zipWith (\ p (d, v) -> map (\pd -> rest (d / fromIntegral pd) +:+ v) p) ps
  . zipWith (\ idx v -> (dur (vs !! ((idx - 1) `mod` (length vs))), v)) [0 ..] 
  $ vs
  where
    ps = (10000 : p) : (repeat p)

herzlichst = 
  har
  . zipWith (\ inst vs -> loop 10 . inst . mel $ vs) instruments
  . shiftPattern pattern
  . zipWith bpm speeds 
  $ voices
  where
    voices = map (uncurry voice) $ [ (tnr, qnr), (qnr, tnr), (tnr, tnr), (qnr, tnr), (qnr, tnr) ]
    pattern = [ 16, 8, 19, 8, 8 ]
    instruments = 
      [ (\x -> voiceOohs {-synthVoice-} $ har [ x, mel [ wnr, enr, x ]])
      , (\x -> synthVoice $ har [ x, mel [ wnr, enr, x ]])
      --, voiceOohs
      --, choirAahs
      -- , {-glockenspiel
      --, pizzicatoStrings
      --, orchestralHarp
      -- , quiet .quiet . electricBassFingered
      -- , marimba
      -- , tinkleBell
      -- , xylophone
      , woodblock . loud 
      , (\ x -> musicBox $ har [ x, loud $ mel [ wnr, qnr, x ] ] )
      , (\ x -> electricBassFingered $ har [ x, quiet $ mel [ wnr, hnr, x] ])
      ]
    speeds = cycle [ 27, 27, 216, 54, 216]

test :: IO ()
test = do
  exportMidi "/tmp/hertzlichst.mid" herzlichst
  system "timidity /tmp/hertzlichst.mid"
  return ()

