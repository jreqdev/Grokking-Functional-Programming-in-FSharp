module Tests.Ch06

open System
open Xunit

open GrokFP.Ch06.TVShows

[<Fact>]
let ``TVShow_Parses_As_Expected_No_SingleYear`` () =
    Assert.Equal(Some(TvShow.make("The Wire", 2002, 2008)), parseShow "The Wire (2002-2008)")
    Assert.Equal(None, parseShow "The Wire aired from 2002 to 2008")
    Assert.Equal(Some(TvShow.make("Breaking Bad", 2008, 2013)), parseShow "Breaking Bad (2008-2013)")
    Assert.Equal(Some(TvShow.make("Mad Men", 2007, 2015)), parseShow "Mad Men (2007-2015)")
    Assert.Equal(Some(TvShow.make("Scrubs", 2001, 2010)), parseShow "Scrubs (2001-2010)")

[<Fact>]
let ``TVShow_Parsing_Fails_When_SingleYear`` () =
    Assert.Equal(None, parseShow "Chernobyl (2021)")
    
[<Fact>]
let ``Extended_TVShow_Parsing_Passes_When_SingleYear`` () =
    Assert.Equal(Some(TvShow.make("Chernobyl", 2021, 2021)), parseShowHandleSingleYear "Chernobyl (2021)")

[<Fact>]
let ``Parsing all or nothing strategy works as expected``() = 
    
    Assert.Equal(
        Some [],
        parseShowsAllOrNothing []
    )

    Assert.Equal(
        Some[TvShow.make("Chernobyl", 2019, 2019) ], 
        parseShowsAllOrNothing ["Chernobyl (2019)"]
    )

    
    Assert.Equal(
        Some[TvShow.make("Chernobyl", 2019, 2019); TvShow.make("Breaking Bad", 2008, 2013)], 
        parseShowsAllOrNothing ["Chernobyl (2019)"; "Breaking Bad (2008-2013)"]
    )
 
    Assert.Equal(
        None,
        parseShowsAllOrNothing ["Chernobyl [2019]"]
    )

    Assert.Equal(
        None,
        parseShowsAllOrNothing ["Chernobyl [2019]"; "Breaking Bad (2008-2013)"]
    )

    Assert.Equal(
        None,
        parseShowsAllOrNothing ["Chernobyl (2019)"; "Breaking Bad"]
    )

open ErrorHandlingWithResult

[<Fact>]
let ``Parsing with Result types works as expected``() = 
    Assert.Equal(
        Ok [],
        parseShows []
    )

    Assert.Equal(
        Ok [TvShow.make("Chernobyl", 2019, 2019) ], 
        parseShows ["Chernobyl (2019)"]
    )

    Assert.Equal(
        Ok [TvShow.make("Chernobyl", 2019, 2019); TvShow.make("Breaking Bad", 2008, 2013)], 
        parseShows ["Chernobyl (2019)"; "Breaking Bad (2008-2013)"]
    )
 
    Assert.Equal(
        Error "Can't extract name from Chernobyl [2019]",
        parseShows ["Chernobyl [2019]"]
    )

    Assert.Equal(
        Error "Can't extract name from Chernobyl [2019]",
        parseShows ["Chernobyl [2019]"; "Breaking Bad"]
    )

    Assert.Equal(
        Error "Can't extract name from Chernobyl [2019]",
        parseShows ["Chernobyl [2019]"; "Breaking Bad (2008-2013)"]
    )
