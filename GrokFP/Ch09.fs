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
                              // compiler will generate a warning, it's safe to ignore or suppress
            }
        helper 


