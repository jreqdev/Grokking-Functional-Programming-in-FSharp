module Tests.Ch07

open System
open Xunit



module FinalRequirementsTests = 

    open GrokFP.Ch07.MusicArtists.CoffeeBreak_maintainability

    let us = Location "U.S."
    let eng = Location "England"

    let artist (name, genre, location, activity) = 
        { Name = name; Genre = genre; Origin = location; YearsActive = activity }

    let period (start, ``end``) = 
        { Start = start; End = ``end`` } : PeriodInYears

    let metallica = artist("Metallica", HeavyMetal, us, StillActive(1981, [])) 
    let ledzeppelin = artist("Led Zeppelin", HardRock, eng, ActiveInPast [ period(1968, 1980) ])
    let beegees = artist("Bee Gees", Pop, eng, ActiveInPast [ period(1958, 2003); period(2009, 2012) ])

    let private artists = [ metallica; ledzeppelin; beegees ]

    let private search conditions = 
        searchArtists(artists, conditions) |> Set.ofSeq
        
    let private test(conditions, expectedResult: Artist seq) = 
        let expected = Set.ofSeq expectedResult
        let actual = search conditions
        Assert.Equal<Set<Artist>>(expected, actual)

    [<Fact>]
    let ``Test Correct Artist Search Results``() = 
        
        test(
            [ SearchByGenre [Pop]
              SearchByOrigin [eng]
              SearchByActiveYears(period(1950, 2022)) ], [beegees] )

        test(
            [ SearchByOrigin [eng]
              SearchByActiveYears(period(1950, 2022)) ], [ledzeppelin; beegees])
        
        test([ SearchByActiveYears(period(1950, 2022)) ], [metallica; ledzeppelin; beegees])

        test([ SearchByActiveYears(period(1983, 2003)) ], [metallica; beegees])

        test([ SearchByActiveYears(period(2019, 2022)) ], [metallica])

        test([ SearchByActiveYears(period(1950, 1959)) ], [beegees])

        test([ SearchByActiveLength(48, 2022) ], [beegees])

        test(
            [ SearchByOrigin [us]
              SearchByActiveLength(48, 2022) ], [])

        test(
            [ SearchByOrigin [us]
              SearchByActiveLength(40, 2022) ], [metallica])

        test(
            [ SearchByOrigin [us]
              SearchByActiveLength(40, 2022) ], [metallica])
        
        test(
            [ SearchByOrigin [us; eng]
              SearchByActiveLength(40, 2022) ], [metallica; beegees])
        


