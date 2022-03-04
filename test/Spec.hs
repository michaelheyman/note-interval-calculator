{-# LANGUAGE OverloadedStrings #-}

import           Parser
import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import           Types

specNote :: Spec
specNote =
  describe "noteParser" $ do
    it "should parse every note" $ do
      parse noteParser "" "C" `shouldParse` NoteC
      parse noteParser "" "C#" `shouldParse` NoteCSharp
      parse noteParser "" "Db" `shouldParse` NoteDFlat
      parse noteParser "" "D" `shouldParse` NoteD
      parse noteParser "" "D#" `shouldParse` NoteDSharp
      parse noteParser "" "Eb" `shouldParse` NoteEFlat
      parse noteParser "" "E" `shouldParse` NoteE
      parse noteParser "" "F" `shouldParse` NoteF
      parse noteParser "" "F#" `shouldParse` NoteFSharp
      parse noteParser "" "Gb" `shouldParse` NoteGFlat
      parse noteParser "" "G" `shouldParse` NoteG
      parse noteParser "" "G#" `shouldParse` NoteGSharp
      parse noteParser "" "Ab" `shouldParse` NoteAFlat
      parse noteParser "" "A" `shouldParse` NoteA
      parse noteParser "" "A#" `shouldParse` NoteASharp
      parse noteParser "" "Bb" `shouldParse` NoteBFlat
      parse noteParser "" "B" `shouldParse` NoteB
    it "fails on 'X's, producing correct error message" $
      parse noteParser "" "X" `shouldFailWith` err 0 (utok 'X' <> elabel "valid note")

specNotes :: Spec
specNotes =
  describe "notesParser" $ do
    it "should parse all single notes" $ do
      parse notesParser "" "C" `shouldParse` [NoteC]
      parse notesParser "" "D" `shouldParse` [NoteD]
      parse notesParser "" "E" `shouldParse` [NoteE]
      parse notesParser "" "F" `shouldParse` [NoteF]
      parse notesParser "" "G" `shouldParse` [NoteG]
      parse notesParser "" "A" `shouldParse` [NoteA]
      parse notesParser "" "B" `shouldParse` [NoteB]
    it "should parse sequence with all C notes" $ do
      parse notesParser "" "CC#" `shouldParse` [NoteC, NoteCSharp]
      parse notesParser "" "C#C" `shouldParse` [NoteCSharp, NoteC]
    it "should parse sequence with all D notes" $ do
      parse notesParser "" "DbDD#" `shouldParse` [NoteDFlat, NoteD, NoteDSharp]
      parse notesParser "" "DbD#D" `shouldParse` [NoteDFlat, NoteDSharp, NoteD]
      parse notesParser "" "DDbD#" `shouldParse` [NoteD, NoteDFlat, NoteDSharp]
      parse notesParser "" "DD#Db" `shouldParse` [NoteD, NoteDSharp, NoteDFlat]
      parse notesParser "" "D#DbD" `shouldParse` [NoteDSharp, NoteDFlat, NoteD]
      parse notesParser "" "D#DDb" `shouldParse` [NoteDSharp, NoteD, NoteDFlat]
    it "should parse sequence with all E notes" $ do
      parse notesParser "" "EbE" `shouldParse` [NoteEFlat, NoteE]
      parse notesParser "" "EEb" `shouldParse` [NoteE, NoteEFlat]
    it "should parse sequence with all F notes" $ do
      parse notesParser "" "FF#" `shouldParse` [NoteF, NoteFSharp]
      parse notesParser "" "F#F" `shouldParse` [NoteFSharp, NoteF]
    it "should parse sequence with all G notes" $ do
      parse notesParser "" "GbGG#" `shouldParse` [NoteGFlat, NoteG, NoteGSharp]
      parse notesParser "" "GbG#G" `shouldParse` [NoteGFlat, NoteGSharp, NoteG]
      parse notesParser "" "GGbG#" `shouldParse` [NoteG, NoteGFlat, NoteGSharp]
      parse notesParser "" "GG#Gb" `shouldParse` [NoteG, NoteGSharp, NoteGFlat]
      parse notesParser "" "G#GbG" `shouldParse` [NoteGSharp, NoteGFlat, NoteG]
      parse notesParser "" "G#GGb" `shouldParse` [NoteGSharp, NoteG, NoteGFlat]
    it "should parse sequence with all A notes" $ do
      parse notesParser "" "AbAA#" `shouldParse` [NoteAFlat, NoteA, NoteASharp]
      parse notesParser "" "AbA#A" `shouldParse` [NoteAFlat, NoteASharp, NoteA]
      parse notesParser "" "AAbA#" `shouldParse` [NoteA, NoteAFlat, NoteASharp]
      parse notesParser "" "AA#Ab" `shouldParse` [NoteA, NoteASharp, NoteAFlat]
      parse notesParser "" "A#AbA" `shouldParse` [NoteASharp, NoteAFlat, NoteA]
      parse notesParser "" "A#AAb" `shouldParse` [NoteASharp, NoteA, NoteAFlat]
    it "should parse sequence with all B notes" $ do
      parse notesParser "" "BbB" `shouldParse` [NoteBFlat, NoteB]
      parse notesParser "" "BBb" `shouldParse` [NoteB, NoteBFlat]


main :: IO ()
main = do
  hspec specNote
  hspec specNotes
