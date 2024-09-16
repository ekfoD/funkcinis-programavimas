# fp-2024


theme => music

this app will be for producers who want to create music. possible actions:
createMelody/editMelody/deleteMelody/readMelody/transposeMelody/changeMelodyTempo:



transposeMelody-> 

createMelody-> id:, note: {<pitch>, <duration>}, note: {<pitch>, <duration>}...

editMelody -> note:                               //you choose id and it prints notes with their corresponding id's. choose what note you want to edit and you edit it

deleteMelody -> id:                         //ir tada ta istrina

readMelody -> id:                           //ir tada ta atprintina


### pvz.:

melodyList:
    output-- 1. A4 A16 D1 G4
             2. B1 G16

readMelody: 
    input-- 1
    output-- A4 A16 D1 G4

deleteMelody:
    input-- 1
    output-- "successfully deleted!"

editMelody:
    input-- 1
    output-- 1. A4, 2. G16 3. B1
    input-- 2 C2
    output-- "successfully edited!"

transposeMelody:
    input-- 1 2
    output-- C4 C16 F1 A4

changeMelodyTempo:
    input-- 1 1
    output-- C8 C16 F2 A8

## my BNF:

<melody> ::= <note> | <note> <melodyID>

<note> ::= <pitch> <duration>

<melodyID> ::= "1 - 99"

<pitch> ::= "C" | "D" | "E" | "F" | "G" | "A" | "B"

<duration> ::= "1" | "2" | "4" | "8" | "16"

