module Tests.Ch10

open System
open Xunit

open GrokFP.Library
open GrokFP.Ch10.Actors


let private testActor(op, values) = 
    async {
        use actor = new SafeUpdateActor<int>(0)
            
        let asyncOps = 
            let update value = actor.Update(op, value)
            values |> Seq.map (update >> Async.retn)

        do! asyncOps |> Async.Parallel |> Async.Ignore
            
        let! value = actor.Value()

        return value   
    }

[<Fact>]
let ``Correct_Values_Parallel_Ops`` () =

    async {
        let testValues = [2; 4; 7; 11; 9; 5; 1; 2; 0;]
        
        let! actual1 = testActor ((+), testValues)

        let! actual2 = 
            let op _ v2 = v2
            testActor (op, testValues)

        Assert.Equal(testValues |> List.sum, actual1)
        Assert.Equal(testValues |> List.last, actual2)
    }
    

