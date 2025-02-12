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

ValuesOnDemand.colorInfiniteSequence 
|> Seq.take 10
|> List.ofSeq
|> printfn "%A"
