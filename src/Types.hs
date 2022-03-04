module Types where

-- https://en.wikipedia.org/wiki/Musical_note#12-tone_chromatic_scale
data Note = NoteC
    | NoteCSharp
    | NoteDFlat
    | NoteD
    | NoteDSharp
    | NoteEFlat
    | NoteE
    | NoteF
    | NoteFSharp
    | NoteGFlat
    | NoteG
    | NoteGSharp
    | NoteAFlat
    | NoteA
    | NoteASharp
    | NoteBFlat
    | NoteB
    deriving (Eq, Show)
