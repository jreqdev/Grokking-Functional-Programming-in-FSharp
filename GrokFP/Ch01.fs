module GrokFP.Ch01

// F# simple function definitions

let increment x = x + 1 // F# will infer x must be an int

let getFirstCharacter (s: string) = s[0] // Sometimes a type annotation is required 

let wordScore word = String.length word // here the compiler will infer word must be a string

