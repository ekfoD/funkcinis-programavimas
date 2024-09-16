# fp-2024


theme => music

this app will be for producers who want to create music. possible actions:
createMelody/editMelody/deleteMelody/readMelody/transposeMelody/changeMelodyTempo:


### Function descriptions:

melodyList:
    input-- <nothing>
    output-- <melodies printed seperated by \\n>

readMelody:
    input-- <melodyID>
    output-- <melody printed>

deleteMelody:
    input-- <melodyID>
    output-- <melody deleted. message that successful>

editMelody:
    input-- <melodyID>
    output-- <that melody notes and every note has a number>    
    input-- <noteID>
    output-- <message that successfully edited and do you want to continue?>
    input-- <yes or no>
    ...

transposeMelody:
    input-- <melodyID> <how many notes you want to transpose (if pitch up, positive number, pitch down - negative)>
    output-- <transposed melody>

changeMelodyTempo:
    input-- <melodyID> <what tempo you want to change (number and its sign means in what side and how much should tempos shift)>
    output-- <melody with changed tempo>

createMelody:
    input-- <melodyID>
    input-- <note>
    output-- <do you need to add more notes?>
    input-- <yes or no>
    ...

### Function examples:

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
    output-- "successfully edited! edit more?"
    input-- no

transposeMelody:
    input-- 1 2
    output-- C4 C16 F1 A4

changeMelodyTempo:
    input-- 1 1
    output-- C8 C16 F2 A8

createMelody:
    input-- 1
    input-- B4
    output-- "do you want to add more notes?"
    input-- no

## my BNF:

<melody> ::= <note> | <note> <melodyID>

<note> ::= <pitch> <duration>

<melodyID> ::= "1 - 99"

<pitch> ::= "C" | "D" | "E" | "F" | "G" | "A" | "B"

<duration> ::= "1" | "2" | "4" | "8" | "16"

