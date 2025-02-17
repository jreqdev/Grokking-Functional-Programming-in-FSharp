﻿module GrokFP.Library

open System.Runtime.CompilerServices
open System.Collections.Generic

let strJoin (sep: string) (values: string seq) = System.String.Join(sep, values)

[<Extension>]
type StringExtensions =
    [<Extension>]
    static member inline Substring2(str: string, startIndex, endIndex) =
        str.Substring(startIndex, endIndex - startIndex)
    
type RandomExtensions = 
    [<Extension>]
    static member inline NextBool(r: System.Random) =
        match r.Next(2) with
        | 0 -> false
        | 1 -> true
        | _ -> failwith "Like Bender would say, I think I've seen a two. Something in .net has gone very badly if this fails here :-/"
        
type DictionaryExtensions =
    [<Extension>]
    static member inline AsMap(dictionary: IDictionary<_, _>) =
        dictionary 
        |> Seq.map (fun keyAndValue -> keyAndValue.Key, keyAndValue.Value) 
        |> Map.ofSeq


type OptionBuilder() = 
    member _.Bind(value, f) = 
        match value with 
        | Option.None -> Option.None
        | Some value -> f(value)

    member _.Return(value) = Some value

let option = OptionBuilder()

type ResultBuilder() = 
    member _.Bind(value, f) = 
        match value with 
        | Result.Error err -> Error err
        | Ok value -> f(value)

    member _.Return(value) = Ok value

let result = ResultBuilder()

type ListExtensions =

    // it'd be nice if this could be turned into a property instead but 
    // can't see a way to do it.
    [<Extension>]
    static member inline Last(x) = x |> List.last

module List = 

    let last = List.last

    let tryLast = List.tryLast

module Result = 

    let fromOption error opt = 
        match opt with
        | Option.None -> Error error
        | Option.Some value -> Ok value

    let orElse ifError result = 
        match result with
        | Result.Ok ok -> Ok ok
        | Result.Error _ -> ifError

    let flatten result =
        match result with
        | Ok (Ok result) -> Ok result
        | Ok (Error err) -> Error err
        | Error err -> Error err
        

module Option = 

    let toResult error opt = 
        match opt with
        | Some value -> Ok value
        | None -> Error error

    let flatten option = 
        match option with
        | Some (Some value) -> Some value
        | Some None -> None
        | None -> None
 
    let iterAsync computation opt : Async<unit> = 
        match opt with
        | Option.Some some -> computation some
        | None -> async { return () }


module Map =
    let mapKeys f map =
        map
        |> Map.toSeq
        |> Seq.map (fun (key, value) -> f key, value)
        |> Map.ofSeq

module Async = 

    let orElse ifException computation = 
        async {
            try return! computation
            with _ -> 
                printfn "Async::orElse recovery"
                return! ifException
        }

    let retn value = async { return value }

    let retn2 funThunk = async { return funThunk() }

    let orElseValue value computation = 
        computation 
        |> orElse (retn value)

    let orElseThunk funThunk computation = 
        computation 
        |> orElse (retn2 funThunk)


    /// Will run the computation at least once and then retry maxRetries times. 
    /// This is a recursive solution that does not leverage the orElse function
    [<TailCallAttribute>]
    let retry maxRetries computation = 

        let rec helper maxTries =         
            if maxTries > 0
            then 
                async {
                    try return! computation
                    with _ -> return! helper (maxTries - 1)
                }
            else computation

        helper maxRetries
        
    /// Will run the computation at least once and then retry maxRetries times. 
    /// This is a fold-based solution
    let retryFold maxRetries computation = 
        [0.. maxRetries - 1]
        |> List.map(fun _ -> computation)
        |> List.fold(fun state retry -> state |> orElse retry) computation 

    let map f computation = 
        async {
            let! r = computation
            return f(r)
        }


    // F# already includes Async.Sequential and Async.Parallel
    // Nevertheless below are a couple of extremely simple implementations
    // of a sequence function

    /// Transforms a list of async computations into a single
    /// async computation of a list value
    /// [ async1<'T>; async2<'T>; async3<'T> ] -> async [ t1; t2; t3 ]
    let private sequenceFold computations = 
        computations 
        |> List.fold
            (fun resultList currentComputation -> 
                async {
                    let! resultList = resultList
                    let! result = currentComputation
                    return result::resultList }) 
            (async { return [] })

    /// Transforms a list of async computations into a single
    /// async computation of a list value
    /// [ async1<'T>; async2<'T>; async3<'T> ] -> async [ t1; t2; t3 ]
    /// Mutable implementation with ResizeArray
    let private sequenceMutable computations = 
        async {
            let results = ResizeArray(computations |> List.length)

            for comp in computations do
                let! computationResult = comp
                results.Add computationResult

            return results |> List.ofSeq
        }
        

module AsyncSeq = 
    
    open FSharp.Control

    /// Wrapping the alternative sequence in a function
    /// helps avoid infinite recursion (eager evaluation)
    let orElse alternativeSeq aSeq = 
        asyncSeq { 
            try yield! aSeq
            with _ -> yield! (alternativeSeq())
        }       

    ///<summary>Returns a sequence yielding sliding windows containing elements from the input
    /// sequence. 
    ///</summary>
    ///<param name="windowSize">The number of elements in each window.</param>
    ///<param name="source">The input async sequence.</param>
    ///<returns>The result async sequence.</returns>
    let windowed (windowSize: int) (source: AsyncSeq<'T>) : AsyncSeq<'T[]> = 
        
        let rec yieldWindow (enumerator: IAsyncEnumerator<'T>) previousWindow = 
            asyncSeq {
                let currentWindow = ResizeArray(windowSize)

                // copy contents of the previous window to the current window, skipping first element
                if previousWindow |> Array.length > 0 
                then
                    previousWindow 
                    |> Array.skip 1
                    |> Array.iter (fun elem -> currentWindow.Add(elem))

                // read from the sequence as many elements as needed to complete
                // the current window
                let! read = enumerator.MoveNext()
                let mutable current = read
                
                while current.IsSome do
                    currentWindow.Add(current.Value)
                    if currentWindow.Count = windowSize
                    then 
                        let resultWindow = currentWindow.ToArray()
                        yield resultWindow
                        yield! yieldWindow enumerator resultWindow
                    else 
                        let! next = enumerator.MoveNext()
                        current <- next
            }

        if windowSize < 1 then invalidArg (nameof(windowSize)) "Window size must be greater than zero"

        asyncSeq {
            use enumerator = source.GetEnumerator()
            yield! yieldWindow enumerator [| |]
        }
        
    ///<summary>
  /// Yields all elements of the second asynchronous sequence and then
  /// all elements of the first asynchronous sequence.
  ///</summary>
    let append2 seq1 seq2 = AsyncSeq.append seq2 seq1