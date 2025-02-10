module GrokFP.Ch09

(*
   - F# List module will throw an exception if zipping two lists of different lengths. Seq will drop values from the longest one
     See https://stackoverflow.com/questions/46994189/why-cant-i-zip-two-lists-of-different-lengths for a reasonable explanation
*)


open System
open GrokFP.Library
open System.Collections.Generic

// I know, numerically this has not much to do with the original Java code. 
// but as long as it can be used to generate numbers it will still illustrate the important points.
let exchangeRatesTableApiCall(currency: string) : IDictionary<string, decimal> = 

    if Random.Shared.NextDouble() < 0.15 then failwith "Connection error"

    if(currency = "USD")
    then 
        [ "EUR", Decimal (0.81 + Random.Shared.NextDouble())
          "JPY", Decimal (103.25 + Random.Shared.NextDouble()) ] |> dict

    else failwith "Rate not available"


type Currency = Currency of string

let exchangeRatesTable(Currency currency) : Async<Map<Currency, decimal>> = 
    async {
        do! Async.Sleep(Random.Shared.Next(15, 40)) // simulate latency

        return 
            exchangeRatesTableApiCall(currency).AsMap() 
            |> Map.mapKeys Currency // simply wrap the key

    }

// Spoiler alert: I think this just solves the problem. Bypasses what the book wants to teach though.
let exchangeIfTrendingRecursiveSolution' getRates (amount: decimal, from: Currency, ``to``: Currency, desiredTimes) : Async<decimal> = 
    
    let rec helper(times, previousRate: decimal) = 
        async {
            try
                printfn $"Trying to get rate. Times: {times} PreviousRate: {previousRate}"
                let! rate = getRates from

                match rate |> Map.tryFind ``to``with 
                | Some rate when rate > previousRate && times + 1 = desiredTimes ->
                    printfn "We're done."
                    return amount * rate
                | Some rate when rate > previousRate -> 
                    printfn $"Found trend: {times} times. Previous rate: {previousRate} Last rate: {rate}"
                    return! helper(times + 1, rate)
                | Some rate -> 
                    printfn $"Found lower value: {rate}, resetting."
                    return! helper(0, 0m)    
                | None -> printfn $"Value not found for {``to``}"; return! helper(0, 0m)    
            with _ -> 
                printfn "Connection error"
                return! helper(0, 0m)
        }

    helper(0, 0m)

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

    let lastRates(from: Currency, ``to``: Currency) : Async<decimal list> = 
        async {
            let! table1 = exchangeRatesTable from |> Async.retry 10
            let! table2 = exchangeRatesTable from |> Async.retry 10
            let! table3 = exchangeRatesTable from |> Async.retry 10

            return 
                [table1; table2; table3] 
                |> List.choose (extractSingleCurrencyRate ``to``) // note that this discards none values so we could return less than 
                                                                  // three elements here!
        }

    let exchangeIfTrending(amount: decimal, from, ``to``) = 
        async { 
            let! lastRates = lastRates(from, ``to``)
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

