// ===============
(* - F# Async and async computation expression block is roughly equivalent to Scala's IO
   - Scala's List.range can be expressed with F# range expressions such as [startHour .. (endHour - lengthHours)]. 
     In F# both starting and ending values are inclusive.
    - A few simple helper functions for Async defined in Library.fs make things a lot easier
      Check out https://github.com/demystifyfp/FsToolkit.ErrorHandling for an actively developed library for error handling
*)
// ===============

module GrokFP.Ch08

open System
open GrokFP.Library

let workingHours = 8, 16

type MeetingTime = { StartHour: int; EndHour: int }
        with
        static member Make(sh, eh) = { StartHour = sh; EndHour = eh }

let calendarEntriesApiCall(name: string) : MeetingTime list = 
    if Random.Shared.NextDouble() < 0.25 then failwith "Connection error"
    else
        let m = MeetingTime.Make
        if name = "Alice" then [m(8, 10); m(11, 12)]
        elif name = "Bob" then [m(9, 10)]
        else [m(Random.Shared.Next(5) + 8, Random.Shared.Next(4) + 13) ]

let createMeetingApiCall(names: string list, time: MeetingTime) : unit = 
    if Random.Shared.NextDouble() < 0.25 then failwith "create meeting API error"
    else printfn "Side-effect: created meeting."

module MeetingSchedulerNoErrorHandling =

    let calendarEntries(name: string) : Async<MeetingTime list> = 
        // like IO.Delay, this block does not get executed when calendarEntries is called
        // it simply creates a computation that may be executed at some point
        async { return calendarEntriesApiCall name }
    
    let scheduledMeetings(person1: string, person2: string) : Async<MeetingTime list> = 
        async {
            let person1Entries = calendarEntriesApiCall person1
            let person2Entries = calendarEntriesApiCall person2
            return person1Entries@person2Entries
        }

module CoffeeBreakWorkingWithValues = 
    
    open MeetingSchedulerNoErrorHandling

    let meetingsOverlap(meeting1: MeetingTime, meeting2: MeetingTime): bool =
        meeting1.EndHour > meeting2.StartHour && meeting2.EndHour > meeting1.StartHour

    let possibleMeetings(existingMeetings: MeetingTime list, startHour, endHour, lengthHours) = 
        [startHour .. (endHour - lengthHours)]
        |> List.map (fun startHour -> { StartHour = startHour; EndHour = startHour + lengthHours })
        |> List.filter(fun slot -> existingMeetings |> List.forall (fun meeting -> not (meetingsOverlap(meeting, slot))))
  
    let schedule(person1: string, person2: string, lengthHours) : Async<MeetingTime option> =
        async {
            let! existingMeetings = scheduledMeetings(person1, person2)
            let possible = possibleMeetings(existingMeetings, fst workingHours, snd workingHours, lengthHours)
            return possible |> List.tryHead
        }

module CardGame_CastTheDie = 
    
    let castTheDie() = 
        if(Random.Shared.NextBool()) 
        then failwith "Die fell off!"
        else Random.Shared.Next(1, 7)

    let drawAPointCard() = 
        if(Random.Shared.NextBool()) 
        then failwith "No cards!"
        else Random.Shared.Next(1, 15)
    
    let answers() = 
        
        let a1 = castTheDie |> Async.retn2 |> Async.orElse (Async.retn 0)

        let a2 = drawAPointCard |> Async.retn2 |> Async.orElse (Async.retn2 castTheDie)

        let a3 = 
            castTheDie 
            |> Async.retn2
            |> Async.orElseThunk castTheDie
            |> Async.orElseValue 0

        let a4 = 
            async {
                let! die = castTheDie |> Async.retn2 |> Async.orElse (Async.retn 0)
                let! card = drawAPointCard |> Async.retn2 |> Async.orElse (Async.retn 0)
                return die + card
            }

        let a5 = 
            async {
                let! card = drawAPointCard |> Async.retn2
                let! die1 = castTheDie |> Async.retn2
                let! die2 = castTheDie |> Async.retn2

                return card + die1 + die2 
            } |> Async.orElse (Async.retn 0)

        [ a1; a2; a3; a4; a5 ]

module WhereShouldWeHandlePotentialFailures =
    
    // Option 1: in calendarEntries. Then schedule does not need to change
    let calendarEntries(name: string) : Async<MeetingTime list> =
        async { return calendarEntriesApiCall name }
        |> Async.orElse (async { return calendarEntriesApiCall name })
        |> Async.orElseValue []

    // Option 2: in the scheduledMeetings helper
    let scheduledMeetings(person1: string, person2: string) : Async<MeetingTime list> = 

        // let's reuse the calendarEntries function defined without error handling
        let calendarEntries = MeetingSchedulerNoErrorHandling.calendarEntries

        async {
            let! person1Entries = 
                calendarEntries person1 
                |> Async.orElse(calendarEntries person1)
                |> Async.orElseValue []
            
            let! person2Entries = 
                calendarEntries person2
                |> Async.orElse(calendarEntries person2)
                |> Async.orElseValue []
            
            return person1Entries@person2Entries
        }
        
    // Option 3: in the schedule function
    let schedule(person1: string, person2: string, lengthHours) : Async<MeetingTime option> =

        // let's re-use the original scheduledMeetings function with no error handling
        let scheduledMeetings = MeetingSchedulerNoErrorHandling.scheduledMeetings

        async {
            let! existingMeetings = 
                scheduledMeetings(person1, person2)
                |> Async.orElse(scheduledMeetings(person1, person2))
                |> Async.orElseValue []

            let possibleMeetings = CoffeeBreakWorkingWithValues.possibleMeetings(existingMeetings, fst workingHours, snd workingHours, lengthHours)
            return possibleMeetings |> List.tryHead
        }

module StoringData = 
    
    let schedulingProgram(getName: Async<string>, showMeeting: MeetingTime option -> Async<unit>) = 
        async {
            let! name1 = getName
            let! name2 = getName
            let hours = 2
            let! possibleMeeting = CoffeeBreakWorkingWithValues.schedule(name1, name2, hours)

            do! showMeeting possibleMeeting  // equivalent to let! _ = showMeeting possibleMeeting
        }

    // curried to use partial function application later
    // in the schedule function
    let createMeeting names time = 
        async { return createMeetingApiCall(names, time) }

    let schedule(person1: string, person2: string, lengthHours: int) : Async<MeetingTime option> = 

        // let's re-use the original scheduledMeetings function with no error handling
        let scheduledMeetings = MeetingSchedulerNoErrorHandling.scheduledMeetings

        async {
            let! existingMeetings = 
                scheduledMeetings(person1, person2)
                |> Async.orElse (scheduledMeetings(person1, person2))
                |> Async.orElseValue []

            let possibleMeeting = 
                CoffeeBreakWorkingWithValues.possibleMeetings(existingMeetings, fst workingHours, snd workingHours, lengthHours)
                |> List.tryHead
            
            // Option.iterAsync is easy to define and allows me to write this one-liner.
            do! possibleMeeting |> Option.iterAsync (createMeeting [person1; person2])

            (* 
            // Alternatively, pattern matching is good too
            do! 
                match possibleMeeting with
                | Some possibleMeeting -> createMeeting [person1; person2] possibleMeeting
                | None -> Async.retn ()
            *)

            return possibleMeeting
        }

module Caching = 

    let cachedCalendarEntries(name: string) : Async<MeetingTime list> = failwith "not implemented"
    let updateCachedEntries(name: string, newEntries: MeetingTime list) : Async<unit> = failwith "not implemented"

    let calendarEntriesWithCache(name: string) : Async<MeetingTime list> = 
        let getEntriesAndUpdateCache = 
            async {
                let! currentEntries = MeetingSchedulerNoErrorHandling.calendarEntries(name)
                do! updateCachedEntries(name, currentEntries)
                return currentEntries
            }

        cachedCalendarEntries(name) |> Async.orElse getEntriesAndUpdateCache

module RetriesAndAnyNumberOfPeopleAttending = 
    
    let calendarEntries(name: string) : Async<MeetingTime list> = 
        async { return calendarEntriesApiCall name }
    
    let scheduledMeetings(attendees: string list) =
        attendees 
        |> List.map (fun attendee -> calendarEntries attendee |> Async.retry 10)
        |> Async.Sequential // built-in but returns an array rather than a list
        |> Async.map (Seq.concat >> List.ofSeq) // flatten the sequences and convert to list

