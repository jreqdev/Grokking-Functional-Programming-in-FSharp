module GrokFP.Ch06.TVShows 

open GrokFP.Library

// ===============
(*
    * computation expressions can be used as an alternative to Scala's 'for comprehension' feature
    * String's substring method in .net uses length as the second argument rather than the position of the endIndex
      therefore Substring2 is used as an extension method
    * Adding elements to the start of the list is a O(1) op, but when used with operations like fold the list order 
      might not be what's expected! List.rev can help with that.
*)
// ===============

type TvShow = { Title: string; Start: int; End: int}
    /// Helper method to construct values 
    with 
    static member make(title, start, ``end``) = 
        { Title = title; Start = start; End = ``end`` }

let sortShows(shows: TvShow list) = 
    shows 
    |> List.sortBy(fun tvShow -> tvShow.End - tvShow.Start) // or List.sortByDescending
    |> List.rev

let toIntOption (str: string) : int option = 
    match System.Int32.TryParse str with
    | true, theInt -> Some theInt
    | _ -> None

let extractYearStart(rawShow: string) : int option = 
    let bracketOpen = rawShow.IndexOf('(')
    let dash        = rawShow.IndexOf('-')
    let yearStrOpt  =
        if (bracketOpen <> -1 && dash > bracketOpen + 1) 
        then Some(rawShow.Substring2(bracketOpen + 1, dash)) 
        else None
    
    // we define our own toIntOption function here
    // and use Option's bind function to compose its result with 
    // the Option value we already had in yearStrOpt
    yearStrOpt |> Option.bind toIntOption

let extractName(rawShow: string): string option = 
    let bracketOpen = rawShow.IndexOf('(')
    if (bracketOpen > 0) 
    then Some(rawShow.Substring(0, bracketOpen).Trim())
    else None
  

// let's implement this one using a computation expression as an alternative to 
// the for-comprehension version
// See https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions
let extractYearEnd(rawShow: string): int option = 
    let dash         = rawShow.IndexOf('-')
    let bracketClose = rawShow.IndexOf(')')

    option {
        let! yearStr = // Note the let! syntax to unwrap the value inside the Option
            if (dash <> -1 && bracketClose > dash + 1) 
            then Some(rawShow.Substring2(dash + 1, bracketClose))
            else None
        let! year = toIntOption yearStr
        return year
    }

let parseShow rawShow : TvShow option =
    option {
        let! name = extractName rawShow
        let! yearStart = extractYearStart rawShow
        let! yearEnd = extractYearEnd rawShow
        return { Title = name; Start = yearStart; End = yearEnd }
    }

  
// The above is roughly equivalent to the following,
// which uses directly two calls to flatMap/bind and one final map call
let parseShow_withBareBindFunctions rawShow : TvShow option = 
    rawShow
    |> extractName 
    |> Option.bind (fun name -> 
        rawShow 
        |> extractYearStart
        |> Option.bind (fun yearStart -> 
            rawShow 
            |> extractYearEnd 
            |> Option.map (fun yearEnd -> { Title = name; Start = yearStart; End = yearEnd })
        )
    )

// let's handle the "single year" case now.    
// ======================================================


let extractSingleYear(rawShow: string): int option = 
    let dash         = rawShow.IndexOf('-')
    let bracketOpen  = rawShow.IndexOf('(')
    let bracketClose = rawShow.IndexOf(')')

    option {
        let! yearStr = 
            if (dash = -1 && bracketOpen <> -1 && bracketClose > bracketOpen + 1)
            then Some(rawShow.Substring2(bracketOpen + 1, bracketClose))
            else None
        let! year = toIntOption yearStr
        return year
    }

let parseShowHandleSingleYear rawShow : TvShow option =
    option {
        let! name = extractName rawShow
        let! yearStart = rawShow |> extractYearStart |> Option.orElse(extractSingleYear rawShow)
        let! yearEnd   = rawShow |> extractYearEnd   |> Option.orElse(extractSingleYear rawShow)
        return { Title = name; Start = yearStart; End = yearEnd }
    }

module PracticingFunctionalErrorHandling = 
    
    let extractSingleYearOrYearEnd(rawShow: string) = 
      extractSingleYear(rawShow) |> Option.orElse(extractYearEnd rawShow)

    let extractAnyYear(rawShow) =
      extractYearStart(rawShow) 
      |> Option.orElse(extractYearEnd rawShow)
      |> Option.orElse(extractSingleYear rawShow)

    let extractSingleYearIfNameExists(rawShow) =
      extractName rawShow |> Option.bind(fun _ -> extractSingleYear(rawShow))

    let extractAnyYearIfNameExists(rawShow) = 
      extractName rawShow |> Option.bind(fun _ -> extractAnyYear(rawShow))

// the original problem was about parsing a list of shows so let's get back to it
// ==================================================================================


let parseShowsDiscardBadOnes(rawShows: string list) : TvShow list = 
    rawShows
    |> List.map parseShowHandleSingleYear
    |> List.map Option.toList
    |> List.concat // Scala code uses flatten here
    
/// This version uses List.choose to discard directly the None values
let parseShowsDiscardBadOnes_Alternative(rawShows: string list) : TvShow list = 
    rawShows |> List.choose parseShowHandleSingleYear
    
let addOrResign(parsedShows: TvShow list option, newParsedShow: TvShow Option) : TvShow list option = 
    option {
        let! parsedShows = parsedShows
        let! newParsedShow = newParsedShow
        return newParsedShow::parsedShows // semantically equivalent: return List.append [newParsedShow] parsedShows
    }

// This is an adaptor helper function. This curried form can be used directly by the fold function
let addOrResignCurried parsedShows newParsedShow = addOrResign(parsedShows, newParsedShow)

let addOrResignWithPatternMatching(parsedShows: TvShow list option, newParsedShow: TvShow Option) : TvShow list option = 
    match parsedShows, newParsedShow with
    | Some parsedShows, Some newParsedShow -> Some (newParsedShow::parsedShows)
    | _ -> None

/// This does not short-circuit the processing when a None value is found
let parseShowsAllOrNothing(rawShows: string list) : TvShow list option = 
    rawShows 
    |> List.map parseShowHandleSingleYear
    |> List.fold addOrResignCurried (Some [])
    |> Option.map List.rev // so that the result list is returned in the original input order. 


// Let's capture error details now: Option is not descriptive enough here
// ======================================================================

// F# does not have Either type, but there's a Result<'T, 'TError> type
// the "right" side is the error type in Result but does not really matter

module ErrorHandlingWithResult =

    type ParseError = string // this is just a compilation alias. we don't pay a performance price here

    let extractName(rawShow: string) = 
        let bracketOpen = rawShow.IndexOf('(')
        if (bracketOpen > 0) 
        then Ok(rawShow.Substring(0, bracketOpen).Trim())
        else Error $"Can't extract name from {rawShow}"

    let extractSingleYear(rawShow: string) : Result<int, ParseError> =
        let dash         = rawShow.IndexOf('-')
        let bracketOpen  = rawShow.IndexOf('(')
        let bracketClose = rawShow.IndexOf(')')

        result {
            let! yearStr = 
                if (dash = -1 && bracketOpen <> -1 && bracketClose > bracketOpen + 1)
                then Ok(rawShow.Substring2(bracketOpen + 1, bracketClose))
                else Error $"Can't extract single year from {rawShow}"
            let! year = toIntOption yearStr |> Option.toResult $"Can't parse {yearStr}"
            return year
        }

    let extractYearStart(rawShow: string) : Result<int, ParseError> =

        let bracketOpen = rawShow.IndexOf('(')
        let dash        = rawShow.IndexOf('-')
        let yearStrResult  =
            if (bracketOpen <> -1 && dash > bracketOpen + 1) 
            then Ok(rawShow.Substring2(bracketOpen + 1, dash)) 
            else Error $"Can't extract start year from {rawShow}"

        // we can do this to unwrap the year as a string from the result
        // and then try to parse it as an int using function composition (>>)
        let endResult = 
            yearStrResult 
            |> Result.bind (toIntOption >> (Result.fromOption $"Can't parse {yearStrResult}"))

        // or alternatively we can write a helper lambda function
        let endResult2 = 
            yearStrResult 
            |> Result.bind (fun yearStr -> 
                yearStr
                |> toIntOption
                |> Option.toResult $"Can't parse {yearStrResult}"
            )

        endResult2

    let extractYearEnd(rawShow: string): Result<int, ParseError> = 
        let dash         = rawShow.IndexOf('-')
        let bracketClose = rawShow.IndexOf(')')

        result {
            let! yearStr = 
                if (dash <> -1 && bracketClose > dash + 1) 
                then Ok(rawShow.Substring2(dash + 1, bracketClose))
                else Error $"Can't extract end year from {rawShow}"
            let! year = toIntOption yearStr |> Option.toResult $"Can't parse {yearStr}"
            return year
        }

    let parseShow(rawShow: string) : Result<TvShow, string> = 
        result {
            let! name = extractName rawShow
            let! yearStart = rawShow |> extractYearStart |> Result.orElse(extractSingleYear rawShow)
            let! yearEnd   = rawShow |> extractYearEnd   |> Result.orElse(extractSingleYear rawShow)
            return { Title = name; Start = yearStart; End = yearEnd }
        }

    let addOrResign(parsedShows: Result<TvShow list, ParseError>) (newParsedShow: Result<TvShow, ParseError>) =
        result {
            let! parsedShows = parsedShows
            let! newParsedShow = newParsedShow
            return newParsedShow::parsedShows
        }

    let parseShows(rawShows: string list) : Result<TvShow list, ParseError> =
        rawShows
        |> List.map parseShow
        |> List.fold addOrResign (Ok [])
        |> Result.map List.rev
    
