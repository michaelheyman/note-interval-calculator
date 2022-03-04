{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Map
import           Data.Text
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import           Types

type Parser = Parsec Void Text

noteParser :: Parser Note
noteParser = choice
    [ NoteCSharp  <$ string "C#"
    , NoteC       <$ string "C"
    , NoteDFlat   <$ string "Db"
    , NoteDSharp  <$ string "D#"
    , NoteD       <$ string "D"
    , NoteEFlat   <$ string "Eb"
    , NoteE       <$ string "E"
    , NoteFSharp  <$ string "F#"
    , NoteF       <$ string "F"
    , NoteGFlat   <$ string "Gb"
    , NoteGSharp  <$ string "G#"
    , NoteG       <$ string "G"
    , NoteAFlat   <$ string "Ab"
    , NoteASharp  <$ string "A#"
    , NoteA       <$ string "A"
    , NoteBFlat   <$ string "Bb"
    , NoteB       <$ string "B" ] <?> "valid note"

notesParser :: Parser [Note]
notesParser = many noteParser
