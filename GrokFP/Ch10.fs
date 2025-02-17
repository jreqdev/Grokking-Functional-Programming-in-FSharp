module GrokFP.Ch10

(* - This chapter also relies quite a bit on Scala specific libraries
     My choices to implement these stream/concurrent ideas on F#
     are Async, AsyncSeq, and actors (as light wrapper types over built-in MailboxProcessor)

     There's a few scattered resources on the web about mailbox processors
     https://fsharp.github.io/fsharp-core-docs/reference/fsharp-control-fsharpmailboxprocessor-1.html 

     but one of my favourite introduction to the subject can be found here:

     https://www.manning.com/books/real-world-functional-programming

     Just keep in mind the book is a bit dated by now, especially the C# parts
*)

open FSharp.Control
open GrokFP.Library
open System.Threading

type City = City of string
    with member this.Value = let (City city) = this in city

type CityStats = { City: City; CheckIns: int }


// process a stream of check-ins

let calculateTopCities (n: int) (citiesWithCheckIns: Map<City, int>) = 
    citiesWithCheckIns 
    |> Map.toSeq 
    |> Seq.sortByDescending(fun (_, checkIns) -> checkIns)
    |> Seq.map (fun (city, checkIns) -> { City =  city; CheckIns = checkIns })
    |> Seq.truncate n // do not use 'take' here or this will blow up when the sequence is empty
    |> List.ofSeq

let printStats (cityStats: CityStats) = sprintf $"CityStats ({cityStats.City}), {cityStats.CheckIns}"

let processCheckIns topCitiesN (checkIns: AsyncSeq<City>) : Async<unit> = 
    (Map.empty, checkIns)
    ||> AsyncSeq.scan
        (fun map city -> 
            let cityCheckIns = map |> Map.tryFind city |> Option.defaultValue 0
            map |> Map.add city (cityCheckIns + 1))
    |> AsyncSeq.map (calculateTopCities topCitiesN)
    |> AsyncSeq.iter (fun topN -> 
        topN 
        |> List.map printStats
        |> strJoin "; "
        |> printfn "%s"
    )

let checkInValues = 
    asyncSeq { 
        City "Sydney"
        City "Sydney" 
        City "Cape Town"
        City "Singapore"
        City "Cape Town"
        City "Sydney"
    }

let extremeCheckInValues =

    let replicateTimes = 100_000

    asyncSeq { 
        City "Sydney"
        City "Dublin" 
        City "Cape Town"
        City "Lima"
        City "Singapore"
    } 
    |> AsyncSeq.replicate replicateTimes
    |> AsyncSeq.concat
    |> AsyncSeq.append2 (AsyncSeq.init replicateTimes (fun i -> City $"{i}" ))
    |> AsyncSeq.append2 (asyncSeq { City "Sydney"; City "Sydney"; City "Lima" })

let processCheckInsBatched bufferSize topCitiesN (checkIns: AsyncSeq<City>) = 
    (Map.empty, checkIns)
    ||> AsyncSeq.scan
        (fun map city -> 
            let cityCheckIns = map |> Map.tryFind city |> Option.defaultValue 0
            map |> Map.add city (cityCheckIns + 1))
    |> AsyncSeq.bufferByCount bufferSize
    |> AsyncSeq.map Seq.last
    |> AsyncSeq.map (calculateTopCities topCitiesN)
    |> AsyncSeq.iter (fun topN -> 
        topN 
        |> List.map printStats
        |> strJoin "; "
        |> printfn "%s"
    )

module Actors = 

    type internal SafeUpdateMsg<'T> = 
        | Update of op: ('T -> 'T -> 'T) * newValue: 'T
        | Update2 of op: ('T -> 'T)
        | Replace of newValue: 'T
        | Get of AsyncReplyChannel<'T>

    type SafeUpdateActor<'T>(initialValue) = 

        let mb = MailboxProcessor<SafeUpdateMsg<_>>.Start (
            fun mbox -> 
                let rec mbLoop mbValue = 
                    async {
                        let! msg = mbox.Receive()

                        match msg with
                        | SafeUpdateMsg.Update (op, newValue) -> 
                            let newValue = op mbValue newValue
                            return! mbLoop newValue
                        | Update2 op -> 
                            let newValue = op mbValue
                            return! mbLoop newValue
                        | Replace newValue -> 
                            //printfn "This is SafeUpdateActor::replace..."
                            return! mbLoop newValue
                        | Get replyChannel -> 
                            replyChannel.Reply mbValue
                            return! mbLoop mbValue
                    }

                mbLoop initialValue
        )

        interface System.IDisposable with
            member _.Dispose(): unit = mb.Dispose()

        member _.Update(op, value) = 
            mb.Post(Update(op, value))

        member _.Update2(op) = 
            mb.Post(Update2 op)

        member _.Replace(value) = 
            mb.Post(Replace(value))

        member _.Value() : Async<'T> = 
            mb.PostAndAsyncReply(Get, timeout = 2000)

        member x.Dispose() = (x :> System.IDisposable).Dispose()

    type internal SafeLoggerActor<'T> = 
        | Display of 'T

    /// A very simple actor for thread-safe logging
    type SafeLogActor<'T>(logger: 'T -> Async<unit>) = 
        
        let mb = MailboxProcessor<SafeLoggerActor<_>>.Start(
            fun mbox -> 
                let rec mbLoop() = 
                    async {
                        let! msg = mbox.Receive()
                        match msg with
                        | SafeLoggerActor.Display(value) -> 
                            return! 
                                async {
                                    do! logger value
                                    return! mbLoop()
                                }
                    }

                mbLoop()
        )

        interface System.IDisposable with
            member _.Dispose(): unit = mb.Dispose()

        member _.Display(value) = mb.Post(Display value)
            

module PracticingConcurrentIOs =

    open System
    open Actors

    let castTheDieImpure() = 
        let value = Random.Shared.Next(0, 6) + 1
        //printfn $"[{value}]"
        value
        
    let castTheDie() = async { return castTheDieImpure() }

    let p1() = 
        async {
            do! Async.Sleep 1000
            return 
                Async.Parallel [ castTheDie(); castTheDie() ] 
                |> Async.map Seq.sum
        }

    let castAndStore(actor: SafeUpdateActor<_>) = 
        async {
            let! cast = castTheDie()
            actor.Update(List.append, [cast])
        }

    let p2() = 
        async {
            use actor = new SafeUpdateActor<List<int>>([])

            do! Async.Parallel [ castAndStore(actor); castAndStore(actor) ] |> Async.Ignore

            return! actor.Value()
        }

    let p3() = 
        async {
            use actor = new SafeUpdateActor<List<int>>([])
            do!
                List.init 3 (fun _ -> castAndStore(actor)) 
                |> Async.Parallel
                |> Async.Ignore
        }

    let p4() = 
        async {
            use actor = new SafeUpdateActor<int>(0)
            do!
                List.init 100 (fun _ -> async {
                    let! cast = castTheDie()
                    if cast = 6 then actor.Update((+), 1)
                }) 
                |> Async.Parallel
                |> Async.Ignore

            return! actor.Value() 
        }

    let p5() =
        (fun i -> async {   
            do! Async.Sleep 1000
            return! castTheDie()
        })
        |> List.init 100
        |> Async.Parallel 
        |> Async.map (Seq.sum)

module ConcurrentCheckIns = 

    open Actors

    type Nothing = unit // simply an alias to tag functions that run indefinitely

    let rec public updateRanking
        (
            storedCheckIns: SafeUpdateActor<Map<City, int>>, 
            storedRanking: SafeUpdateActor<List<CityStats>>
        ) : Async<Nothing> = 
        async {
            let! currentCheckIns = storedCheckIns.Value()
            let ranking = calculateTopCities 3 currentCheckIns
            storedRanking.Replace(ranking)

            do! updateRanking(storedCheckIns, storedRanking)
        }
        
    let public updateCheckIns(storedCheckIns: SafeUpdateActor<Map<City, int>>) city : unit =         
        storedCheckIns.Update2(
            fun map ->
                let cityCheckIns = map |> Map.tryFind city |> Option.defaultValue 0
                map |> Map.add city (cityCheckIns + 1)
            )

    let rec private printRankings(storedRanking: SafeUpdateActor<_>, displayRanking: SafeLogActor<_>) : Async<Nothing> = 
        async {
            let! currentRanking = storedRanking.Value()
            displayRanking.Display(currentRanking)
            do! Async.Sleep 1000
            return! printRankings(storedRanking, displayRanking)
        }

    let processCheckIns (checkIns: AsyncSeq<City>) : Async<Nothing> = 
        async {

            // start the actors

            use storedCheckIns = new SafeUpdateActor<Map<City, int>>(Map.empty)
            use storedRanking = new SafeUpdateActor<List<CityStats>>([])
            use displayRanking = 
                new SafeLogActor<List<CityStats>>(
                    fun valueToDisplay -> async { 
                        for cityStats in valueToDisplay do
                            printf $"[{cityStats.City.Value}: {cityStats.CheckIns}] "
                        printfn ""
                    }
                )

            // define the programs or computations
            // the checkIn map is updated with each element of the sequence
            // the other two run indefinitely, one to recalculate the rankings
            // and another one to display them

            let rankingProgram = updateRanking(storedCheckIns, storedRanking)
            let checkInsProgram = checkIns |> AsyncSeq.iter (updateCheckIns(storedCheckIns))
            let printRankingProgram = printRankings(storedRanking, displayRanking)

            // now run them in parallel
            do! 
                [ rankingProgram
                  checkInsProgram
                  printRankingProgram ] 
                  |> Async.Parallel
                  |> Async.Ignore
                   
            printfn "We never get here since two of the three async computations never actually end"

            return ()
        }

    // let's say now we don't want to hardcode the behaviour that prints the rankings to the console

    let printToConsole = 
        fun valueToDisplay -> async { 
            for cityStats in valueToDisplay do
                printf $"[{cityStats.City.Value}: {cityStats.CheckIns}] "
            printfn ""
        }

    // processCheckIns2 accepts a parameter to customise the logging/printing behaviour

    let processCheckIns2 logger (checkIns: AsyncSeq<City>) : Async<Nothing> = 
        async {

            // start the actors

            use storedCheckIns = new SafeUpdateActor<Map<City, int>>(Map.empty)
            use storedRanking = new SafeUpdateActor<List<CityStats>>([])
            use displayRanking = new SafeLogActor<List<CityStats>>(logger)

            // define the programs or computations
            // the checkIn map is updated with each element of the sequence
            // the other two run indefinitely, one to recalculate the rankings
            // and another one to display them

            let rankingProgram = updateRanking(storedCheckIns, storedRanking)
            let checkInsProgram = checkIns |> AsyncSeq.iter (updateCheckIns(storedCheckIns))
            let printRankingProgram = printRankings(storedRanking, displayRanking)

            // now run them in parallel
            do! 
                [ rankingProgram
                  checkInsProgram
                  printRankingProgram ] 
                  |> Async.Parallel
                  |> Async.Ignore
                   
            printfn "We never get here since two of the three async computations never actually end"

            return ()
        }

    let processCheckInsWithLog1 checkIns = processCheckIns2 printToConsole checkIns

    

module AsynchronousAccess = 

    // let's prepare things so that a client/consumer can ask for an update whenever it desires
    // rather than the producer directly reporting or emitting the results

    // it can be sufficient to write a small actor that takes care of producing a result value
    // whenever client asks and this actor is returned from the concurrent computation

    open Actors

    type ResultReportingActorMsg = 
        | GetValue of AsyncReplyChannel<List<CityStats>>

    type ResultReportingActor(rankingActor: SafeUpdateActor<List<CityStats>>) = 
        
        let mb = MailboxProcessor<ResultReportingActorMsg>.Start(fun mbox -> 
            let rec loop() = 
                async {
                    let! msg = mbox.Receive()
                    
                    match msg with
                    | ResultReportingActorMsg.GetValue replyChannel -> 
                        do!
                            async {
                                let! currentRanking = rankingActor.Value()
                                replyChannel.Reply(currentRanking)
                            }

                    return! loop()
                }
        
            loop()
        )

        interface System.IDisposable with
            member _.Dispose(): unit = mb.Dispose()

        member _.CurrentRanking() : Async<List<CityStats>> = 
            mb.PostAndAsyncReply(GetValue, timeout = 3000)

        member x.Dispose() = (x :> System.IDisposable).Dispose()
    
    let processCheckIns (checkIns: AsyncSeq<City>) = //: Async<ResultReportingActor> = 

        let updateRanking = ConcurrentCheckIns.updateRanking 
        let updateCheckIns = ConcurrentCheckIns.updateCheckIns

        async {

            // start the actors. Don't dispose of them when the method returns
            // or nothing won't work!

            // ideally clean-up would happen when/if a consumer decides to cancel/stop 
            // the whole process so this function can be wrapped in a Disposable type
            // that can call dispose on all the agents

            let storedCheckIns = new SafeUpdateActor<Map<City, int>>(Map.empty)
            let storedRanking = new SafeUpdateActor<List<CityStats>>([])
            let resultReporting = new ResultReportingActor(storedRanking)

            // define the programs or computations
            // the checkIn map is updated with each element of the sequence
            // the other two run indefinitely, one to recalculate the rankings
            // and another one to display them

            let rankingProgram = updateRanking(storedCheckIns, storedRanking)
            let checkInsProgram = checkIns |> AsyncSeq.iter (updateCheckIns(storedCheckIns))
            
            // now run them in parallel and schedule to run in the thread pool
            // don't wait for the result

            let parallelProcessing = 
                [ rankingProgram
                  checkInsProgram ]
                    |> Async.Parallel
                    |> Async.Ignore
                    
            return parallelProcessing, resultReporting
        }

    let checkInsClient(checkIns) = 
        // let's ask for a report 10 times
        async {
            let! concurrentComputation, reporter = processCheckIns checkIns 

            let cts = new CancellationTokenSource()

            Async.Start(concurrentComputation, cts.Token)

            do!
                List.init 10 (fun _ -> async {
                    let! current = reporter.CurrentRanking()
                    do! Async.Sleep(1000)
                    printfn $"%A{current}" 
                })
                |> Async.Sequential
                |> Async.Ignore

            cts.Cancel()
        }
        

module ProperDisposeAndCancellation = 
    
    open Actors

    // let's improve on the previous implementation by providing a proper wrapper type
    // for the concurrent computation that can do automatic cleanup via IDisposable
    // Also all the actors are nicely encapsulated inside this type as well 
    // since it's an internal implementation details that doesn't have to be leaked to clients

    type CheckInsProcessor() = 
        
        let storedCheckIns = new SafeUpdateActor<Map<City, int>>(Map.empty)
        let storedRanking = new SafeUpdateActor<List<CityStats>>([])
        let resultReporting = new AsynchronousAccess.ResultReportingActor(storedRanking)
        let cts = new CancellationTokenSource()

        interface System.IDisposable with
            member _.Dispose(): unit = 
                storedCheckIns.Dispose()
                storedRanking.Dispose()
                resultReporting.Dispose()
                cts.Cancel()
                (cts :> System.IDisposable).Dispose()

        /// Note that this returns unit now since the method returns immediately
        /// after the concurrent computation has been scheduled in the threadpool
        member _.ProcessCheckIns (checkIns: AsyncSeq<City>) =

            let updateRanking = ConcurrentCheckIns.updateRanking 
            let updateCheckIns = ConcurrentCheckIns.updateCheckIns

            // define the programs or computations
            // the checkIn map is updated with each element of the sequence
            // the other two run indefinitely, one to recalculate the rankings
            // and another one to display them

            let rankingProgram = updateRanking(storedCheckIns, storedRanking)
            let checkInsProgram = checkIns |> AsyncSeq.iter (updateCheckIns(storedCheckIns))
            
            // now run them in parallel and schedule to run in the thread pool
            // don't wait for the result

            let parallelProcessing = 
                [ rankingProgram
                  checkInsProgram ]
                    |> Async.Parallel
                    |> Async.Ignore
                    
            // start computation and don't wait, fire and forget style
            Async.Start(parallelProcessing, cts.Token)
            
        member _.GetRanking() = 
            resultReporting.CurrentRanking()


    // now let's define a consumer to test things

    let checkInsClient(checkIns) = 
        // let's ask for the current rankings 10 times
        async {
            use checkInsProcessor = new CheckInsProcessor()

            checkInsProcessor.ProcessCheckIns(checkIns)

            do!
                List.init 10 (fun _ -> async {
                    let! current = checkInsProcessor.GetRanking()
                    do! Async.Sleep(1000)
                    printfn $"%A{current}" 
                })
                |> Async.Sequential
                |> Async.Ignore
        }

        

