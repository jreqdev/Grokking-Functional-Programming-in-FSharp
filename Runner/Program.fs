open FSharp.Control

open GrokFP.Ch09

(*
    This is just a simple way of trying things directly in a console program
*)

let usd = Currency "USD"
let eur = Currency "EUR"

//exchangeIfTrendingRecursiveSolution(1m, usd, eur, 3) |> Async.RunSynchronously |> printfn "%A"

//WorkingBottomUp.exchangeIfTrendingInfiniteRecursive  WorkingBottomUp.lastRates_lastThreeRates (10m, usd, eur)
//|> Async.RunSynchronously 
//|> printfn "%A"

//WorkingBottomUp.lastRates(false, usd, eur, 5) |> Async.RunSynchronously |> printfn "%A"

//ValuesOnDemand.colorInfiniteSequence 
//|> Seq.take 10
//|> List.ofSeq
//|> printfn "%A"

//StreamsAsSequenceOfAsyncValues.asyncList
//|> Async.RunSynchronously
//|> printfn "%A"

//StreamsAsSequenceOfAsyncValues.eightNumbers ()
//|> printfn "%A"

//StreamsAsSequenceOfAsyncValues.eightNumbersStackOverflow ()
//|> printfn "%A"

//StreamsAsSequenceOfAsyncValues.castNTimes 50
//|> printfn "%A"

//StreamsAsSequenceOfAsyncValues.filteredCast (fun i -> i > 4) 50
//|> printfn "%A"

//StreamsAsSequenceOfAsyncValues.PracticingStreamOperations.a4_until5_and_two_more
//|> printfn "%A"

//StreamsAsSequenceOfAsyncValues.PracticingStreamOperations.a5_100_values_then_discard()
//|> Async.RunSynchronously
//|> printfn "%A"

//StreamsAsSequenceOfAsyncValues.PracticingStreamOperations.a7_cast_until_n_6s_in_a_row(3)
//|> printfn "%A"

//let rateList = 
//    SolvingCurrencyProblemWithStreams.rates(usd, eur) 
//    |> AsyncSeq.take 100
//    |> AsyncSeq.toListSynchronously

//rateList |> List.iteri (fun i elem -> printfn $"{i + 1} - {elem}")


SolvingCurrencyProblemWithStreams.exchangeIfTrendingWithDelayBetweenCalls (3000, 10m, usd, eur)
|> AsyncSeq.toArraySynchronously
|> printfn "%A"