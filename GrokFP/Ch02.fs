module GrokFP.Ch02

module ShoppingCart = 

    let getDiscountPercentage(items: string list) : int =
        if List.contains "Book" items then 5 else 0

    let getDiscountPercentage'(items: string list) : int =
        // functions such as List.contains commonly take the list parameter last
        // so it can be pipelined, like this:
        if items |> List.contains "Book" then 5 else 0

module TipCalculator = 

    let getTipPercentage(names: string list) : int = 
        if (names.Length > 5) then 20
        elif (names.Length > 0) then 10
        else 0

module Other = 

    let getFirstCharacter s = 
        if s |> String.length > 0 then s[0] else ' '

