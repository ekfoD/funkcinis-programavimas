# fp-2024


## lab 3 README.md

### Ability to process batches!

- syntax to do it: `BEGIN query1; query2 END`

- there can be many queries. What matters is that they would be seperated using "; "


### Ability to save and load current state!

- if you want to save, just write "save" and it will save current state into a file "melody_state.txt"

- for loading, just write "load" and it will overwrite the current state with data from the file "melody_state.txt"

## Original README.md

### What is this:

this app will be for producers who want to create music. possible actions:
melodyList/readMelody/deleteMelody/editMelody/transposeMelody/changeMelodyTempo/createMelody:


### How it works and where is the recursion:

- User of this app can create melodies using commands. After creating, user can modify and alter that melody in different ways

- Melody is either a single note, or a collection of other melodies. so, if we would try to represent it in other programming language:
    - `a` is a melody, other characters represent either other melodies or notes => `a = [b, c[e, f, g[h, j, k, l[...]]], d]
    - one melody can have infinite amount of other melodies inside

### Other BNF elements:

- smallInteger => if you want to change melody pitch, you need to write a sign (should the pitch be higher or lower?) and also a number (how many tones should the app alter?). same thing will be used with tempo altering

- terminate => this app design is made in a way that the app will stop doing something only if the user says it

- pitch and duration => i chose these notes/duration intervals because, in my opinion, in musical world you encounter these the most. no flats or sharps because it would really complicate the app

- IDs (note and melody) => this is the simplest and the easiest way to handle identity of similar objects

### Function descriptions:

```
melodyList:
    input-- 
    output-- (melodies printed seperated by \\n)

readMelody:
    input-- <melodyID>
    output-- (melody printed)

deleteMelody:
    input-- <melodyID>
    output-- (melody deleted. message that successful)

editMelody:
    input-- <melodyID>
    output-- (that melody notes and every note has a number)
    input-- <editNotes>
    output-- (editted melody)

transposeMelody:
    input-- <melodyID> <smallInteger> (if pitch up, positive number, pitch down - negative)
    output-- (transposed melody)

changeMelodyTempo:
    input-- <melodyID> <smallInteger> (number and its sign means in what side and how much should tempos shift)
    output-- (melody with changed tempo)

createMelody:
    (if it matches with already existing melodyID, it will overwrite that)
    input-- <melodyID> <addNotes>   (if you typed terminate, it stops) 
    output-- (melody created)
```

### Function examples:

`melodyList`:
    output-- 1. A4 A16 D1 G4
             2. B1 G16

`readMelody`: 
    input-- 1
    output-- A4 A16 D1 G4

`deleteMelody`:
    input-- 1
    output-- "successfully deleted!"

`editMelody`:
    input-- 1
    output-- 1. A4, 2. G16, 3. F4, 4. F2                            <!-- A4(G16(F4)F2) -->
    input-- 2 C2G2 stop
    output-- A4(C2G2(F4)F2)

`transposeMelody`:
    input-- 1 +2
    output-- C4 C16 F1 A4

`changeMelodyTempo`:
    input-- 1 -1
    output-- C8 C16 F2 A8

`createMelody`:
    input-- createMelody 1 B4 C1 (melody F2 F8) stop
    output-- "melody created"



### Small change to BNF and how app works:

1. BNF changed -> `<addNotes> ::= <terminate> | <note> <addNotes>` to `<addNotes> ::= <terminate> | <melody> <addNotes>`
    in order so i could implement recursion with my parsers

2. Changed README.md -> clarified some parts and updated so it would represent updated BNF


### Small change to melody editing:

1. In edit section, now you choose which melody part you want to change, not individual notes