module GrokFP.Ch04

open GrokFP.Library 

type ProgrammingLanguage = { Name: string; Year: int } // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/records

let javaLang = { Name = "Java"; Year = 1995 } // F# will infer that javaLang is a value of type ProgrammingLanguage

let scalaLang : ProgrammingLanguage = { Name = "Scala"; Year = 2004 } // a type annotation can help if ambiguities between records should arise

let languages = [ javaLang; scalaLang ] // very nice syntax to define lists. https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/lists

let names = languages |> List.map (fun lang -> lang.Name)
let names2 = languages |> List.map (_.Name)

let young = languages |> List.filter (fun lang -> lang.Year > 2000)
//let young2 = languages |> List.filter (_.Year > 1) // this won't compile so the long lambda version above is the best we can do for now it seems

module ReturningFunctions  =

    let largerThan (n: int) = fun i -> i > n

    let large = [5; 1; 2; 4; 0] |> List.filter (largerThan 4)

    let divisibleBy (n: int) = fun i -> i % n = 0

    let odds = [5; 1; 2; 4; 15] |> List.filter (divisibleBy 5)

    let shorterThan n = fun s -> s |> String.length < n

    let longWords = ["scala"; "ada"] |> List.filter (shorterThan 4)

    let numberOfS(s: string) = s.Length - s.Replace("s", "").Length

    let containsS moreThan : string -> bool = fun s -> numberOfS s > moreThan

    let withLotsS  = ["rust"; "ada"] |> List.filter (containsS 2)

    module Currying = 

        let largerThan n i = i > n

        let large = [5; 1; 2; 4; 0] |> List.filter (largerThan 4)

        // etc



module WordScoring = 

    let score (word: string) = word.Replace("a", "").Length
    let bonus (word: string) = if word.Contains 'c' then 5 else 0
    let penalty (word: string) = if word.Contains 's' then 7 else 0

    let rankedWords1(wordScore, words) = // wordScore is inferred to be a function of the appropriate type. Words also inferred to be a list

        let negativeScore (word: string) = - wordScore(word)

        words |> List.sortBy negativeScore
    
    /// automatic generalization at play here. Without any explicit constraint, this function is generic
    /// and does not depend on string for anything!
    /// https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/generics/automatic-generalization
    let rankedWords2(wordScore, words) = 
        words |> List.sortBy wordScore |> List.rev

    let highScoringWords(wordScore: string -> int, words: string list) : string list = 
        words |> List.filter(fun word -> wordScore(word) > 1)

    let result = highScoringWords((fun w -> score(w) + bonus(w) - penalty(w)), ["ada"; "haskell"; "scala"; "java"; "rust"])

    let highScoringWord_returning_function(wordScore: string -> int, words: string list) : int -> string list = 
        fun higherThan -> 
            words |> List.filter(fun word -> wordScore(word) > higherThan)

    let highScoringWord_returning_function_that_returns_function
        (
            wordScore: string -> int, words: string list
        ) : int -> string list -> string list = 

        fun higherThan -> 
            fun words -> 
                words |> List.filter(fun word -> wordScore(word) > higherThan)

    // type annotations are not really needed.
    let highScoringWords_curring wordsScore higherThan words =
        words |> List.filter (fun word -> wordsScore(word) > higherThan)
      
    let cumulativeScore(wordScore: string -> int, words) = 
        (0, words) ||> List.fold(fun total word -> total + wordScore (word)) // the (||>) operator can apply a function to two values.
                                                                             // used here to pass an init value of 0 to List.fold
                                                                             // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/