module GrokFP.Ch09

#nowarn "40"

(*
   - F# List module will throw an exception if zipping two lists of different lengths. Seq will drop values from the longest one
     See https://stackoverflow.com/questions/46994189/why-cant-i-zip-two-lists-of-different-lengths for a reasonable explanation
   - Closest equivalent to the Scala Stream type as used by the book is probably AsyncSeq. 
     https://fsprojects.github.io/FSharp.Control.AsyncSeq/
   - Implemented orElse function for AsyncSeq. Since the "else" function used in the example is recursive,
     it caused infinite recursive calls no matter what and therefore stack overflow. Wrapping it in a function
     solves the issue. Better suggestions welcome!
*)


open System
open GrokFP.Library
open System.Collections.Generic

// I know, numerically this has not much to do with the original Java code. 
// but as long as it can be used to generate numbers it will still illustrate the important points.
let exchangeRatesTableApiCall(currency: string) : IDictionary<string, decimal> = 

    if Random.Shared.NextDouble() < 0.20
    then 
        let msg = "Connection error" 
        printfn "%s "msg
        failwith msg

    if(currency = "USD")
    then 
        [ "EUR", Decimal (0.81 + Random.Shared.NextDouble())
          "JPY", Decimal (103.25 + Random.Shared.NextDouble()) ] |> dict

    else failwith "Rate not available"


type Currency = Currency of string

let exchangeRatesTable(Currency currency) : Async<Map<Currency, decimal>> = 
    async {
        return 
            exchangeRatesTableApiCall(currency).AsMap() 
            |> Map.mapKeys Currency // simply wrap the key
    }

/// Spoiler alert: I think this recursive function just solves the problem (well other than
/// the fact that it still making API calls too quickly without any delays)
/// This however bypasses what the book wants to teach though.
let exchangeIfTrendingRecursiveSolution' getRates (amount: decimal, from: Currency, ``to``: Currency, desiredTimes) : Async<decimal> = 
    
    let rec helper(times, soFar) = 

        // what to do when API returns an error or the desired currency is not found
        // this could be a parameter in real-life code rather than hard-coded here
        let decisionWhenErrorOrValueMissing = 

            let keepCurrentTrend = true

            if keepCurrentTrend
            then soFar
            else []

        async {
            try
                let previousRate = soFar |> List.tryHead |> Option.defaultValue 0m

                printfn $"Trying to get rate. Times: {times} PreviousRate: {previousRate}"
                let! rate = getRates from

                match rate |> Map.tryFind ``to``with 
                | Some rate when rate > previousRate && times + 1 = desiredTimes ->
                    let soFar = rate::soFar
                    printfn $"We're done. Found: %A{soFar |> List.rev}" 
                    return amount * rate
                | Some rate when rate > previousRate -> 
                    printfn $"Found trend: {times} times. Previous rate: {previousRate} Last rate: {rate}"
                    return! helper(times + 1, rate::soFar)
                | Some rate -> 
                    printfn $"Found lower value: {rate}, resetting."
                    return! helper(0, [])    
                | None -> printfn $"Value not found for {``to``}"; return! helper(0, decisionWhenErrorOrValueMissing)    
            with _ -> 
                printfn "Connection error"
                return! helper(0, decisionWhenErrorOrValueMissing)
        }

    helper(0, [])

let exchangeIfTrendingRecursiveSolution (amount: decimal, from: Currency, ``to``: Currency, desiredTimes) = 
    exchangeIfTrendingRecursiveSolution' exchangeRatesTable (amount, from, ``to``, desiredTimes)


module WorkingBottomUp = 
    
    let trending(rates: decimal list) : bool = 
        rates.Length > 1 &&
        (Seq.skip 1 rates) 
        |> Seq.zip rates 
        |> Seq.forall (fun (prevRate, rate) -> rate > prevRate) // F# tuple destructuring is nice

    let extractSingleCurrencyRate currencyToExtract (table: Map<Currency, decimal>) : decimal option = 
        table |> Map.tryFind currencyToExtract

    let lastRates_lastThreeRates(from: Currency, ``to``: Currency) : Async<decimal list> = 
        async {
            let! table1 = exchangeRatesTable from |> Async.retry 10
            let! table2 = exchangeRatesTable from |> Async.retry 10
            let! table3 = exchangeRatesTable from |> Async.retry 10

            return 
                [table1; table2; table3] 
                |> List.choose (extractSingleCurrencyRate ``to``) // note that this discards none values so we could return less than 
                                                                  // three elements here!
        }

    let exchangeIfTrending(amount: decimal, from, ``to``) : Async<decimal option> = 
        async { 
            let! lastRates = lastRates_lastThreeRates(from, ``to``)
            return 
                if trending lastRates 
                then Some (amount * lastRates.Last()) 
                else None
        }

    /// Injecting the lastRates function as a parameter allows flexibility here (for unit testing etc)
    let exchangeIfTrendingInfiniteRecursive lastRatesF (amount: decimal, from: Currency, ``to``: Currency) = 
        
        let rec helper(callsCount) = 
            async { 
                let! lastRates = lastRatesF(from, ``to``)
                printfn $"{lastRates} {callsCount}"
                return!
                    if trending lastRates 
                    then async { return {| Result = (amount * lastRates.Last()); FunctionCallsCount = callsCount |} }
                    else helper(callsCount + 1)
            }

        helper(1)

    /// Returns last rate, retrying as many times as necessary. Potentially can run forever..
    let currencyRate(from: Currency, ``to``: Currency) : Async<decimal> =
        let rec helper() = 
            async {
                try 
                    let! rates = exchangeRatesTable from
                    match rates |> extractSingleCurrencyRate ``to`` with
                    | Some rate -> return rate
                    | None -> return! helper()
                with _ -> return! helper()
            }

        helper()
                
            
    let lastRates(log: bool, from: Currency, ``to``: Currency, n: int) : Async<decimal list> = 
        
        let rec helper(soFar) =
            async {
                if log then printfn $"helper soFar: %A{soFar}"

                let! rate = currencyRate(from, ``to``)
                
                let soFar = rate::soFar // add current rate to the accumulated list
                
                if soFar.Length = n 
                then return soFar // we're done
                else return! helper(soFar) // recursive calls
            }
            
        if n < 1 then [] |> Async.retn
        else helper []

module ValuesOnDemand = 

    type Colour = 
        | Yellow
        | Orange
        | Blue
        | Green
        | Black
        | White

    /// lazily evaluated, infinite sequence of colours.
    /// In C# this could be done with iterators (yield return and IEnumerable)
    let colorInfiniteSequence = 
        let rec helper =
            seq {
                yield! [ Yellow; Orange; Blue; Green; Black; White]
                yield! helper // this is where the sequence restarts from the beginning
                              // compiler will generate a warning, it's safe to ignore or suppress with #nowarn "40"
            }

        helper 

module StreamsAsSequenceOfAsyncValues = 

    open FSharp.Control
    
    let stream1 = asyncSeq { 1; 2; 3 }

    let stream2 = asyncSeq { 4; 5; 6 }

    let stream3 = AsyncSeq.append stream1 stream2

    let asyncList: Async<int list> = stream3 |> AsyncSeq.toListAsync

    /// This does not work as intended and will blow up the stack.
    let rec numbers_stack_overflow() = 
        AsyncSeq.append (asyncSeq { 1; 2; 3 }) (numbers_stack_overflow())

    /// This works as expected. Code is structurally similar to the colorInfiniteSequence function above
    let rec numbers() = 
        asyncSeq {
            yield! asyncSeq { 1; 2; 3 }
            yield! numbers()
        }
    
    // calling this function will result in stack overflow
    let eightNumbersStackOverflow() : int list = numbers_stack_overflow() |> AsyncSeq.take 8 |> AsyncSeq.toListSynchronously

    let eightNumbers() : int list = numbers() |> AsyncSeq.take 8 |> AsyncSeq.toListSynchronously

    let castTheDieImpure() = 
        let value = Random.Shared.Next(0, 6) + 1
        printfn $"castTheDie: {value}"
        value
        
    let castTheDie() = async { return castTheDieImpure() }

    // the async computation gets re-evaluated for each element of the sequence.
    let infiniteCastTheDie = AsyncSeq.replicateInfiniteAsync (castTheDie())

    let castNTimes n = infiniteCastTheDie |> AsyncSeq.take n |> AsyncSeq.toListSynchronously

    // Take n elements that satisfy the predicate.
    let filteredCast pred n = 
        infiniteCastTheDie 
        |> AsyncSeq.filter pred 
        |> AsyncSeq.take n
        |> AsyncSeq.toListSynchronously

    module PracticingStreamOperations = 
        
        let a1_filterOddNumbers() = 
            castTheDie() 
            |> AsyncSeq.replicateInfiniteAsync
            |> AsyncSeq.filter (fun i -> i % 2 = 0)
            |> AsyncSeq.take 3

        let a2_firstFive() = 
            castTheDie() 
            |> AsyncSeq.replicateInfiniteAsync
            |> AsyncSeq.take 5
            |> AsyncSeq.map (fun i -> if i = 6 then 12 else i)

        let a3_sum_first_three() = 
            castTheDie() 
            |> AsyncSeq.replicateInfiniteAsync
            |> AsyncSeq.take 3
            |> AsyncSeq.sum

        let a4_until5_and_two_more() = 
            asyncSeq {
                let infSeq = castTheDie() |> AsyncSeq.replicateInfiniteAsync
                let first5 = 
                    infSeq
                    |> AsyncSeq.filter(fun i -> i = 5)
                    |> AsyncSeq.take 1

                let nextTwo = infSeq |> AsyncSeq.take 2

                yield! first5
                yield! nextTwo
            } |> AsyncSeq.toListSynchronously
        
        let a5_100_values_then_discard() =
            castTheDie() 
            |> AsyncSeq.replicateInfiniteAsync
            |> AsyncSeq.take 100
            |> AsyncSeq.iter(fun _ -> ()) // as intended but it's not elegant...
            //|> AsyncSeq.fold (fun _ _ -> []) [] |> Async.Ignore // alternative (even worse?)

        let a6_first_three_casts_next_three_tripled() = 
            asyncSeq {
                let infSeq = castTheDie() |> AsyncSeq.replicateInfiniteAsync
                let first3 = infSeq |> AsyncSeq.take 3
                let next = infSeq |> AsyncSeq.take 3 |> AsyncSeq.map (fun i -> i * 3)
                yield! first3
                yield! next
            } |> AsyncSeq.toListSynchronously
            
        let a7_cast_until_n_6s_in_a_row n = 
            (0, castTheDie() |> AsyncSeq.replicateInfiniteAsync) 
            ||> AsyncSeq.scan (fun sixes current -> if current = 6 then sixes + 1 else 0)
            |> AsyncSeq.filter ((=) n) // point-free style: concise syntax but hides the name! still readable?
            |> AsyncSeq.take(1)
            |> AsyncSeq.toListSynchronously
            
module SolvingCurrencyProblemWithStreams = 
    
    open FSharp.Control

    let extractSingleCurrencyRate = WorkingBottomUp.extractSingleCurrencyRate
    let trending = Array.toList >> WorkingBottomUp.trending

    /// Infinite sequence of rates
    let rates(from: Currency, ``to``: Currency) : AsyncSeq<decimal> = 

        let rec ratesHelper(from: Currency, ``to``: Currency) = //: AsyncSeq<decimal> = 
            exchangeRatesTable(from)
            |> AsyncSeq.replicateInfiniteAsync
            |> AsyncSeq.choose (extractSingleCurrencyRate ``to``)
            |> AsyncSeq.orElse (fun () -> ratesHelper(from, ``to``)) // fun() wrapper needed here
            
        ratesHelper(from, ``to``)

    let exchangeIfTrending(amount: decimal, from: Currency, ``to``: Currency) : AsyncSeq<decimal> = 
        rates(from, ``to``)
        |> AsyncSeq.windowed 3
        |> AsyncSeq.filter trending
        |> AsyncSeq.map Array.last
        |> AsyncSeq.take 1
        |> AsyncSeq.map (fun last -> last * amount)

    
    let exchangeIfTrendingWithDelayBetweenCalls(delayInMs: int, amount: decimal, from: Currency, ``to``: Currency) : AsyncSeq<decimal> = 
        rates(from, ``to``)
        |> AsyncSeq.zip (AsyncSeq.intervalMs delayInMs)
        |> AsyncSeq.map (
            fun (timestamp, rate) -> 
                printfn $"[{timestamp}] {rate}"
                rate)
        |> AsyncSeq.windowed 3
        |> AsyncSeq.filter trending
        |> AsyncSeq.map Array.last
        |> AsyncSeq.take 1
        |> AsyncSeq.map (fun last -> last * amount)

