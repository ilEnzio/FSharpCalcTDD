module Tests


open System
open System.IO

open System.Text
open System
open Xunit
open FsUnit 
open NUnit.Framework

open Calculator

// [<Fact(Skip = "Test")>]
// let ``My test`` () =
//     Assert.True(true)

[<SetUp>]
let calc = {
    State = 0
    Data = []
    LastNum = 0
    LastOp = fun x -> (fun x -> x)
    }


type OutStr(sb:StringBuilder, orig:TextWriter) =
    inherit TextWriter()
    override x.Encoding = stdout.Encoding
    override x.Write (s:string) = sb.Append s |> ignore; orig.Write s
    override x.WriteLine (s:string) = sb.AppendLine s |> ignore; orig.WriteLine s
    override x.WriteLine() = sb.AppendLine() |> ignore; orig.WriteLine()
    member x.Value with get() = sb.ToString()
    static member Create () =
        let orig = stdout
        let out = new OutStr(StringBuilder(), orig)
        Console.SetOut(out)
        out
    interface IDisposable with member x.Dispose() = Console.SetOut(orig)


[<Fact>]
let ``Test Console Menu Output is correct`` () = 
    let prompt = "You may enter a Digit or operations: +, -, /, and =, a C (for clear), and an AC (for clear all)\n"
    
    let withOutStr' f a =
        use out = OutStr.Create()
        f(a)
        out.Value
    
    let withOutStr'' f a = 
        let out = OutStr.Create()
        f(a)
        out.Value

    let captured' = withOutStr'' promptUserMenu ()
        
    Assert.Equal(prompt, captured')

[<Fact>]
let ``Test Calculator can have a numerical state`` () = 
    let newCalc = {calc with State = 10;}
    
    Assert.Equal(10, newCalc.State)

[<Fact>]
let ``Test Calculator only stores appropriate input`` () = 

    let calc = getUserInput calc "3"
    let calc = getUserInput calc "5"
    let calc = getUserInput calc "7"
    let calc = getUserInput calc "+"
    let calc = getUserInput calc "t"
    let calc = getUserInput calc "888"
    
    let testInput = ["888"; "+"; "7"; "5";"3" ]
    
    Assert.Equal<string list> (testInput, calc.Data)

[<Fact>]
let ``Test Calculator can add and update state`` () = 
    let calc = getUserInput calc "28"

    Assert.Equal(28, calc.State)

    let calc = getUserInput calc "+"
    let calc = getUserInput calc "4"

    Assert.Equal(32, calc.State)

[<Fact>]
let ``Calculator can subtract and update state`` () = 
    let calc = getUserInput calc "28"

    let calc = getUserInput calc "-"
    let calc = getUserInput calc "4"

    Assert.Equal(24, calc.State)

[<Fact>]
let ``Test Calculator can multiple and update state`` () = 
    let calc = getUserInput calc "28"

    let calc = getUserInput calc "-"
    let calc = getUserInput calc "4"
    let calc = getUserInput calc "*"
    let calc = getUserInput calc "2"

    Assert.Equal(48, calc.State)

[<Fact>]
let ``Test Calculator can divide and update state`` () = 
    let calc = getUserInput calc "28"

    let calc = getUserInput calc "-"
    let calc = getUserInput calc "4"
    let calc = getUserInput calc "*"
    let calc = getUserInput calc "2"
    let calc = getUserInput calc "/"
    let calc = getUserInput calc "4"

    Assert.Equal(12, calc.State)

[<Fact>]
let ``Test Calculator can accept equal`` () = 

    let calc = getUserInput calc "28"
    let calc = getUserInput calc "="
    let calc = getUserInput calc "="

    let tempData = ["28"]
    
    Assert.Equal(28, calc.State)
    Assert.Equal<string list> (tempData, calc.Data)


    let calc = getUserInput calc "+"
    let calc = getUserInput calc "="
    let tempData' = ["+"; "28"]
    
    Assert.Equal(56, calc.State)
    Assert.Equal<string list> (tempData', calc.Data)
    // Assert.Equal(op, calc.LastOp)  // why is this not equal?

    let calc = getUserInput calc "/"
    let calc = getUserInput calc "8"
    let tempData'' = ["8"; "/"; "+"; "28"]
    
    Assert.Equal(7, calc.State)
    Assert.Equal<string list> (tempData'', calc.Data)


