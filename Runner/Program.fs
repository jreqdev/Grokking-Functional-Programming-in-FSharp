open GrokFP.Ch09

(*
    This is just a simple way of trying things directly in a console program
*)



let usd = Currency "USD"
let eur = Currency "EUR"

//exchangeIfTrendingRecursiveSolution(1m, usd, eur, 5) |> Async.RunSynchronously |> printfn "%A"


WorkingBottomUp.exchangeIfTrendingInfiniteRecursive 
    WorkingBottomUp.lastRates
    (10m, usd, eur)
|> Async.RunSynchronously 
|> printfn "%A"