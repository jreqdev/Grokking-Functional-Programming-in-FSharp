// ===============
(*
    * Scala has zero-cost wrapper types (opaques). We don't have that in F# but we can use single case DU
      Alternatively, we can use structs as simple wrappers
    * Other than that, everything in this chapter can be expressed quite well in F#
*)
// ===============


module GrokFP.Ch07.MusicArtists

module PrimitiveTypes =

    type Artist = { Name: string; Genre: string; Origin: string; YearsActiveStart: int; IsActive: bool; YearsActiveEnd: int }

    let searchArtists (artists: Artist list, genres: string list, locations: string list, searchByActiveYears: bool, activeAfter: int, activeBefore: int) = 
        artists
        |> List.filter (
            fun artist -> 
                (genres.IsEmpty|| genres |> List.contains(artist.Genre)) &&
                (locations.IsEmpty || locations |> List.contains(artist.Origin)) &&
                (not searchByActiveYears || (artist.IsActive || artist.YearsActiveEnd >= activeAfter) &&
                  artist.YearsActiveStart <= activeBefore)
            )

module WithTypes = 

    // Single case DU as an alternative to Scala's opaque types
    type Location = Location of string
        with member this.Value = let (Location loc) = this in loc
    
    // another alternative
    [<Struct>]
    type Location'(value: string) = 
        member _.Value = value
    
    type Genre = Genre of string
        with member this.Value = let (Genre g) = this in g

    type YearsActiveStart = YearsActiveStart of int
        with member this.Value = let (YearsActiveStart start) = this in start

    type YearsActiveEnd = YearsActiveEnd of int
        with member this.Value = let (YearsActiveEnd e) = this in e

    type Artist = 
        { Name: string
          Genre: Genre
          Origin: Location
          YearsActiveStart: YearsActiveStart
          IsActive: bool
          YearsActiveEnd: YearsActiveEnd }

    let searchArtists (artists: Artist list, genres: string list, locations: string list, searchByActiveYears: bool, activeAfter: int, activeBefore: int) = 
        artists
        |> List.filter (
            fun artist -> 
                (genres.IsEmpty|| genres |> List.contains(artist.Genre.Value)) &&
                (locations.IsEmpty || locations |> List.contains(artist.Origin.Value)) &&
                (not searchByActiveYears || (artist.IsActive || artist.YearsActiveEnd.Value >= activeAfter) &&
                  artist.YearsActiveStart.Value <= activeBefore)
            )

module UsingOptionTypes = 

    type Location = Location of string
        with member this.Value = let (Location loc) = this in loc

    type Artist = 
        { Name: string
          Genre: string
          Origin: Location
          YearsActiveStart: int
          YearsActiveEnd: int option }

    let searchArtists (artists: Artist list, genres: string list, locations: string list, searchByActiveYears: bool, activeAfter: int, activeBefore: int) = 
        artists
        |> List.filter (
            fun artist -> 
                (genres.IsEmpty|| genres |> List.contains(artist.Genre)) &&
                (locations.IsEmpty || locations |> List.contains(artist.Origin.Value)) &&
                (not searchByActiveYears 
                    || (artist.YearsActiveEnd |> Option.forall(fun activeEnd -> activeEnd >= activeAfter) &&
                        artist.YearsActiveStart <= activeBefore)
                )
            )

module CoffeeBreak_forall_exists_contains = 

    // I don't think there is a way to make these lambdas shorter to write. Scala's syntax is more concise here

    type User = { Name: string; City: string option; favouriteArtists: string list }

    // 1. users that haven't specified their city or live in Melbourne
    let f1 (users: User list) : User list = 
        users |> List.filter (fun user -> Option.forall ((=) "Melbourne") user.City)
    
    // 2. users that live in Lagos
    let f2 (users: User list) : User list = 
        users |> List.filter (fun user -> Option.contains "Lagos" user.City)
    
    // 3. users that like Bee Gees
    let f3 (users: User list) = 
        users |> List.filter (fun user -> List.contains "Bee Gees" user.favouriteArtists)

    // 4. users that live in cities that start with a letter T
    let f4 (users: User list) = 
        users |> List.filter (fun user -> user.City |> Option.exists (fun city -> city.StartsWith 'T'))

    // 5. users that only like artists that have a name longer than 8 characters (or no favorite artists at all)
    let f5 (users: User list) = 
        users |> List.filter(fun user -> user.favouriteArtists |> List.forall (fun artist -> artist.Length > 8))

    // 5. users users that like some artists whose names start with M
    let f6 (users: User list) = 
        users |> List.filter(fun user -> user.favouriteArtists |> List.exists (fun artist -> artist.StartsWith 'M'))

module OnceConceptInASingleProductType = 
    
    type Location = Location of string
        with member this.Value = let (Location loc) = this in loc

    type PeriodInYears = { Start: int; End: int option }

    type Artist = 
        { Name: string
          Genre: string
          Origin: Location
          YearsActive: PeriodInYears }

    let searchArtists (artists: Artist list, genres: string list, locations: string list, searchByActiveYears: bool, activeAfter: int, activeBefore: int) = 
        artists
        |> List.filter (
            fun artist -> 
                (genres.IsEmpty|| genres |> List.contains(artist.Genre)) &&
                (locations.IsEmpty || locations |> List.contains(artist.Origin.Value)) &&
                (not searchByActiveYears 
                    || (artist.YearsActive.End |> Option.forall(fun activeEnd -> activeEnd >= activeAfter) &&
                        artist.YearsActive.Start <= activeBefore)
                )
            )

module FinitePossibilities = 
    
    type MusicGenre = 
        | HeavyMetal
        | Pop
        | HardRock

    type Location = Location of string
        with member this.Value = let (Location loc) = this in loc

    type PeriodInYears = { Start: int; End: int option }

    type Artist = 
        { Name: string
          Genre: MusicGenre
          Origin: Location
          YearsActive: PeriodInYears }

    let searchArtists (artists: Artist list, genres: MusicGenre list, locations: string list, searchByActiveYears: bool, activeAfter: int, activeBefore: int) = 
        artists
        |> List.filter (
            fun artist -> 
                (genres.IsEmpty|| genres |> List.contains(artist.Genre)) &&
                (locations.IsEmpty || locations |> List.contains(artist.Origin.Value)) &&
                (not searchByActiveYears 
                    || (artist.YearsActive.End |> Option.forall(fun activeEnd -> activeEnd >= activeAfter) &&
                        artist.YearsActive.Start <= activeBefore)
                )
            )

module SumTypesWithCases = 
    
    // See https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions

    type MusicGenre = 
        | HeavyMetal
        | Pop
        | HardRock

    type Location = Location of string
        with member this.Value = let (Location loc) = this in loc

    type YearsActive = 
        | StillActive of since: int // 'since' is just a label
        | ActiveBetween of start: int * ``end``: int // 'since' and 'end' are just labels

    // another possibility, using anonymous records as DU case values
    // See https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/anonymous-records
    [<RequireQualifiedAccess>]
    type YearsActive' = 
        | StillActive of {| Since: int |} 
        | ActiveBetween of {| Start: int; End: int |} 

    type Artist = 
        { Name: string
          Genre: MusicGenre
          Origin: Location
          YearsActive: YearsActive }

    let metallica   = { Name = "Metallica"; Genre = HeavyMetal; Origin = Location "U.S."; YearsActive = StillActive(1981) }
    let ledZeppelin = { Name = "Led Zeppelin"; Genre = HardRock; Origin = Location "England"; YearsActive = ActiveBetween(1968, 1980) }

    let wasArtistActive(artist: Artist, yearStart: int, yearEnd: int) = 
        match artist.YearsActive with
        | StillActive since -> since <= yearEnd
        | ActiveBetween(start, ``end``) -> start <= yearEnd && ``end`` >= yearStart


module SumTypesWithCases_Behaviour =

    open SumTypesWithCases

    let wasArtistActive(artist: Artist, yearStart: int, yearEnd: int) = 
        match artist.YearsActive with
        | StillActive since -> since <= yearEnd
        | ActiveBetween(start, ``end``) -> start <= yearEnd && ``end`` >= yearStart

    let searchArtists (artists: Artist list, genres: MusicGenre list, locations: string list, searchByActiveYears: bool, activeAfter: int, activeBefore: int) = 
        artists
        |> List.filter (
            fun artist -> 
                (genres.IsEmpty|| genres |> List.contains(artist.Genre)) &&
                (locations.IsEmpty || locations |> List.contains(artist.Origin.Value)) &&
                (not searchByActiveYears || wasArtistActive(artist, activeAfter, activeBefore))
            )

    let activeLength(artist: Artist, currentYear: int) = 
        match artist.YearsActive with
        | StillActive since -> currentYear - since
        | ActiveBetween(start, ``end``) -> ``end`` - start


module PlaylistModeling = 

    type Artist = { Name: string }

    type User = { Name: string }

    type Song = { Artist: Artist; Name: string}

    type MusicGenre = 
        | House
        | Funk
        | HipHop
  
    type PlaylistKind = 
        | CuratedByUser of User
        | BasedOnArtist of Artist
        | BasedOnGenres of Set<MusicGenre>

    type Playlist = { Name: string; Kind: PlaylistKind; Songs: Song list }


    let gatherSongs (playlists: Playlist list, artist: Artist, genre: MusicGenre) : Song list = 
        
        // helper function. let's use pattern matching here.
        let matchingSongsFromPlaylist (pl: Playlist) = 
            match pl.Kind with
            | CuratedByUser _ -> pl.Songs |> List.filter (fun song -> song.Artist = artist)
            | BasedOnArtist plArtist when plArtist = artist -> pl.Songs
            | BasedOnGenres plGenres when plGenres.Contains genre -> pl.Songs
            | _ -> []    

        // using List.collect is much simpler here. Am I missing something?

        let solution1_using_collect = playlists |> List.collect matchingSongsFromPlaylist

        let solution2_using_fold = 
            // using the ||> operator here to pass both the inital state and the original list in a single tuple
            ([], playlists)
            ||> List.fold (fun accumulatedSongs playlist -> List.append accumulatedSongs (matchingSongsFromPlaylist playlist))

        solution2_using_fold


module ModelingBehaviours = 
    
    open SumTypesWithCases

    type SearchCondition = 
        | SearchByGenre of MusicGenre list
        | SearchByOrigin of Location list
        | SearchByActiveYears of start: int * ``end``: int

    let searchArtists (artists: Artist list, conditions: SearchCondition list) = 
        artists 
        |> List.filter (
            fun artist -> 
                conditions 
                |> List.forall(fun condition -> 
                    match condition with
                    | SearchByGenre genres -> genres |> List.contains(artist.Genre)
                    | SearchByOrigin locations -> locations |> List.contains(artist.Origin)
                    | SearchByActiveYears(start, ``end``) -> wasArtistActive(artist, start, ``end``)
                )
        )

module CoffeeBreak_maintainability = 


    type PeriodInYears = { Start: int; End: int } 

    type YearsActive = 
        | StillActive of since: int * past: PeriodInYears list
        | ActiveInPast of PeriodInYears list

    type MusicGenre = 
        | HeavyMetal
        | Pop
        | HardRock

    type Location = Location of string
        with member this.Value = let (Location loc) = this in loc

    type Artist = 
        { Name: string
          Genre: MusicGenre
          Origin: Location
          YearsActive: YearsActive }

    let periodOverlapsWithPeriods(checkedPeriod: PeriodInYears, periods: PeriodInYears list ) = 
        periods |> List.exists(fun p -> p.Start <= checkedPeriod.End && p.End >= checkedPeriod.Start)

    let wasArtistActive(artist: Artist, searchPeriod: PeriodInYears) = 
        match artist.YearsActive with
        | StillActive (since, previous)  -> since <= searchPeriod.End || periodOverlapsWithPeriods(searchPeriod, previous)
        | ActiveInPast periods           -> periodOverlapsWithPeriods(searchPeriod, periods)

    // Behaviours


    type SearchCondition = 
        | SearchByGenre of MusicGenre list
        | SearchByOrigin of Location list
        | SearchByActiveYears of PeriodInYears
        | SearchByActiveLength of howLong: int * until: int

    let activeLength(artist: Artist, currentYear: int) : int =
        let periods = 
            match artist.YearsActive with
            | StillActive (since, previousPeriods) -> { Start = since; End = currentYear }::previousPeriods
            | ActiveInPast(periods)                -> periods

        periods 
        |> List.map (fun p -> p.End - p.Start)
        |> List.sum


    let searchArtists (artists: Artist list, conditions: SearchCondition list) = 
        artists 
        |> List.filter (
            fun artist -> 
                conditions 
                |> List.forall(fun condition -> 
                    match condition with
                    | SearchByGenre genres -> genres |> List.contains(artist.Genre)
                    | SearchByOrigin locations -> locations |> List.contains(artist.Origin)
                    | SearchByActiveYears(period) -> wasArtistActive(artist, period)
                    | SearchByActiveLength(howLong, until) -> activeLength(artist, until) >= howLong
                )
        )