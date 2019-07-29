module Main where

import Euterpea
import Euterpea.IO.MIDI.ToMidi2
import Composition1

main :: IO ()
main = writeMidi2 "composition1.mid" comp1
