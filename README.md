# fp-2024


theme => music

this app will be for producers who want to create music. possible actions:

createMelody/editMelody/deleteMelody/readMelody:



createMelody-> id:, note: {<pitch>, <duration>}, note: {<pitch>, <duration>}...

editMelody -> note:                               //you choose id and it prints notes with their corresponding id's. choose what note you want to edit and you edit it

deleteMelody -> id:                         //ir tada ta istrina

readMelody -> id:                           //ir tada ta atprintina

my BNF:


<melody> ::= <note> | <note><melody>

m = A
m = ABA

m = (cool ABBA),BACH,CHAD

<note> ::= <pitch> <duration>

<pitch> ::= "C" | "D" | "E" | "F" | "G" | "A" | "B"

<duration> ::= "1" | "2" | "4" | "8" | "16"

