module GrokFP.Ch05

open GrokFP.Library

type Book = { Title: string; Authors: string list }
type Movie = { Title: string }

let books1 = 
    [ { Title = "FP in Scala"; Authors = ["Chiusano"; "Bjarnason"] }
      { Title = "The Hobbit"; Authors = ["Tokien"] }
      { Title = "Modern Java in Action"; Authors = ["Urma"; "Fusco"; "Mycroft"] } ]


// pipelining is very common in F#
let scalaBooksQty1 = 
    books1 
    |> List.map (_.Title) 
    |> List.filter (fun title -> title.Contains "Scala")
    |> List.length


let books2 = 
    [ { Title = "FP in Scala"; Authors = ["Chiusano"; "Bjarnason"] }
      { Title = "The Hobbit"; Authors = ["Tolkien"] } ]

let bookAdaptations(author: string): Movie list = 
    if author = "Tolkien"
    then [ {Movie.Title = "An Unexpected Journey" }; { Movie.Title = "The Desolation of Smaug" } ]
    else []
  
let a1 = books2 |> List.map _.Authors

let a2 = books2 |> List.map _.Authors |> List.concat // flatten

let a3 = books2 |> List.collect _.Authors

let authors = [ "Chiusano"; "Bjarnason"; "Tolkien" ]

let movieLists = authors |> List.map bookAdaptations 

let b1 = movieLists |> List.concat 

let c1 = 
    books2 
    |> List.collect (fun book -> 
        book.Authors 
        |> List.collect (fun author -> 
            author
            |> bookAdaptations
            |> List.map (fun movie -> $"You may like {movie.Title} because your liked {author}'s {book.Title}")    
        )
    )

// Scala's for comprehensions can be translated to the following.
// Scala is a bit more consistent here since the same for/yield syntax can be used with Option, IO, etc
// while F# relies on computation expressions
let c2 = 
    [ for book in books2 do 
      for author in book.Authors do
      for movie in bookAdaptations(author) do 
      yield $"You may like {movie.Title} because your liked {author}'s {book.Title}" ]

module Events = 
    
    type Event = { Name: string; Start: int; End: int }

    let parseAdHoc(name: string, start: int, ``end``: int) = 
        if name.Length > 0 && ``end`` < 3000 && start <= ``end`` 
        then { Name = name; Start = start; End = ``end`` }
        else Unchecked.defaultof<Event> // F# really does not like creating null values

    let parse(name: string, start: int, ``end``: int) = 
        if name.Length > 0 && ``end`` < 3000 && start <= ``end`` 
        then Some { Name = name; Start = start; End = ``end`` }
        else None

    let validateName(name: string) = if name.Length > 0 then Some(name) else None

    let validateEnd(``end``) = if ``end`` < 3000 then Some(``end``) else None

    let validateStart(start, ``end``) = if start <= ``end`` then Some(start) else None

    let validateLength(start, ``end``, minLength) = 
      if ``end`` - start >= minLength then Some(``end`` - start) else None

    // option computation expression used here. This is defined in the Library.fs file
    // more on this on Ch06.fs

    let parseLongEvent (name: string, start: int, ``end``: int, minLength) = 
        option { 
            let! validName = validateName name
            let! validEnd = validateEnd ``end``
            let! validStart = validateStart(start, ``end``)
            let! _ = validateLength(start, ``end``, minLength) // return value is discarded. It will still break the pipeline if it's None though!

            return { Name = validName; Start = validStart; End = validEnd }
        }

module Points = 

    type Point = { X: int; Y: int } 

    let points = 
        [1] 
        |> List.collect (
            fun x -> [-2; 7] |> List.map (fun y -> { X = x; Y = y })
        )

    let points2 = 
        let xs = [1]
        let ys = [-2; 7]

        // xs and ys can't be accessed outside the scope of points2
        // very useful for defining intermediate values

        [ for x in xs do
          for y in ys do
          yield { X = x; Y = y } ]

    type Point3d = { X: int; Y: int; Z: int } 

    let points3 = 
        let xs = [1]
        let ys = [-2; 7]
        let zs = [3; 4]

        [ for x in xs do
          for y in ys do
          for z in zs do 
          yield { X = x; Y = y; Z = z } ]


module PointsInsideCircles = 

    type Point = { X: int; Y: int } 

    let isInside(point: Point, radius: int) = 
        radius * radius >= point.X * point.X + point.Y * point.Y

    let point (x, y) = { X = x; Y = y } // nothing wrong with creating helper functions

    let points   = [ point(5, 2); point(1, 1) ] 
    let radiuses = [2; 1]

    let statements = 
        [ for r in radiuses do
          for point in points do 
          yield $"{point} is within a radius of {r}: {isInside(point, r)}" ]

    let statements_with_filtering = 
        [ for r in radiuses do 
          for point in points do 
          if isInside(point, r) then yield $"{point} is within a radius of {r}: {isInside(point, r)}" ]

    let statements_with_guard = 
        [ for r in radiuses do 
          
          // this is OK as long as there's not too many conditions, otherwise the nesting soon gets nasty
          if r > 0 then 
              for point in points do 
              if isInside(point, r) then yield $"{point} is within a radius of {r}: {isInside(point, r)}" ]

    let statements_with_validation =

        let validateRadius(radius) = if (radius > 0) then [radius] else []

        [ for r in radiuses do 
          for validR in validateRadius r do
          for point in points do 
          if isInside(point, validR) then yield $"{point} is within a radius of {r}: {isInside(point, r)}" ]

module MoreComprehensions = 
    
    // we have to use sequence comprehensions here. then the result can be converted into a list
    let sequenceMix = 
        seq { 
            for x in [1; 2; 3] do
            for y in set [1] do
            for z in set [0] do
            yield x * y * z
        } |> List.ofSeq // force evaluation of a lazy sequence
