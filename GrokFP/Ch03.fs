module GrokFP.Ch03

open GrokFP.Library 

// substring2 is defined in the GrokFP.Library file


let abbreviate (name: string) =     
    let initial = name.Substring2(0, 1)
    let separator = name.IndexOf ' '
    let lastName = name.Substring(separator + 1)
    $"{initial}. {lastName}" // https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/interpolated-strings

let replan(plan: string list, newCity: string, beforeCity: string) = 
    
    let beforeCityIndex = 
        plan 
        |> List.findIndex (fun cityInPlan -> cityInPlan = beforeCity) // I use a predicate here to find the index by comparing strings
                                                                      // See https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/lambda-expressions-the-fun-keyword

    let citiesBefore = plan |> List.take beforeCityIndex
    let citiesAfter = plan |> List.skip beforeCityIndex
    citiesBefore@[newCity]@citiesAfter // I build a new list with just ''newCity'' here
                                       // Then I use the @ operator to concatenate the three lists while preserving the correct 
                                       // order of elements


    