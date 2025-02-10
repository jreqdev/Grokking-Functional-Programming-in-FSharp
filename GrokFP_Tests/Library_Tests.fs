module Tests.Library

open System
open Xunit

open GrokFP.Library

[<Fact>]
let ``Result flatten... flattens`` () =
    Assert.Equal(Ok "Something", Ok (Ok "Something") |> Result.flatten)
    Assert.Equal(Error "some error", Ok (Error "some error") |> Result.flatten)
    Assert.Equal(Error "err", Error "err" |> Result.flatten)

[<Fact>]
let ``Option flatten... flattens`` () =
    Assert.Equal(Some "Something", Some (Some "Something") |> Option.flatten)
    Assert.Equal(None, Some (None) |> Option.flatten)

    
[<Fact>]
let ``Async Recovery OK - One level of recovery``() = 

    let explodes = 
        async {
            failwith "this will explode"
            return "will never get here"
        }

    async {
        let! result = explodes |> Async.orElse (async { return "recovery" })
        Assert.Equal("recovery", result)
    }

[<Fact>]
let ``Async Recovery OK - Two levels of recovery``() = 

    let explodes = 
        async {
            failwith "this will explode"
            return "will never get here"
        }

    let recoveryExplodesToo =  async { failwith "this will explode too" ; return "never gets here" }

    async {
        let! result = 
            explodes 
            |> Async.orElse recoveryExplodesToo
            |> Async.orElse (async { return "recovery" })

        Assert.Equal("recovery", result)
    }

let assertThrowsAsyncException<'T when 'T :> exn> computation = 
    Assert.ThrowsAsync<'T>(fun () -> (computation |> Async.StartAsTask :> System.Threading.Tasks.Task))

[<Fact>]
let ``Async Recovery Fails - when all orElse fail``() = 

    let explodes = 
        async {
            failwith "this will explode"
            return "will never get here"
        }

    let recoveryExplodesToo =  async { failwith "this will explode too" ; return "never gets here" }

    let andThisOneToo =  async { failwith "and this will explode too" ; return "never gets here" }
    
    let computation = 
        explodes 
        |> Async.orElse recoveryExplodesToo 
        |> Async.orElse andThisOneToo
    
    assertThrowsAsyncException<Exception>(computation)
    
[<Fact>]
let ``Async Recovery is tried maxRetries eventually fails when maxRetries reached``() = 

    let mutable calledTimes = 0

    // let's make it always fail for this test
    let nondeterministicServiceSimulator() = 
        calledTimes <- calledTimes + 1
        let r = Random.Shared.Next(0, 10)
        if r >= 0
        then raise (InvalidOperationException())
    
    let explodes =     
        async {
            nondeterministicServiceSimulator()
            return "If still here, we got lucky"
        }

    task {
        let computation = explodes |> Async.retry 10
        let! ex = assertThrowsAsyncException<InvalidOperationException>(computation)
        Assert.Equal(11, calledTimes)
        Assert.Equal(ex.GetType(), typeof<InvalidOperationException>)
    }

[<Fact>]
let ``Async Recovery is tried maxRetries eventually fails when maxRetries reached - Fold version``() = 

    let mutable calledTimes = 0

    // let's make it always fail for this test
    let nondeterministicServiceSimulator() = 
        calledTimes <- calledTimes + 1
        let r = Random.Shared.Next(0, 10)
        if r >= 0
        then raise (InvalidOperationException())
    
    let explodes =     
        async {
            nondeterministicServiceSimulator()
            return "If still here, we got lucky"
        }

    task {
        let computation = explodes |> Async.retryFold 10
        let! ex = assertThrowsAsyncException<InvalidOperationException>(computation)
        Assert.Equal(11, calledTimes)
        Assert.Equal(ex.GetType(), typeof<InvalidOperationException>)
    }
    
    
    
    


