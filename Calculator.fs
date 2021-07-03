module Calculator

// open System
// open System.Collections.Generic

type Calculator = {
    State : int
    Data: list<string>
    LastNum: int
    LastOp: int -> int -> int
    
    }

let opsDict = dict [
        "+" , (+); 
        "-", (-); 
        "/", (/); 
        "*", (*)
        ] 

type UserInput = 
    | Numeric of string
    | ArithemticOperator of string
    | Command of string
    | Invalid 


let performCalculation calc = 
    printfn "%A, %A, %A" calc.State calc.LastOp calc.LastNum
    calc.LastOp calc.State calc.LastNum

let updateState calc input  = 
    match calc.Data.Length with
    | 1 -> {calc with State = int input}
    |_ -> {calc with State = performCalculation calc}

// let isOperatorOrNumberOrEqual x =
//     opsDict.Keys |> Seq.contains x,  System.Int32.TryParse x|> fst,  x = "="
   
let InputType x = 
    match x with 
    | x when opsDict.Keys |> Seq.contains x -> ArithemticOperator x 
    | x when System.Int32.TryParse x|> fst -> Numeric x
    | x when x = "=" -> Command x 
    | _ -> Invalid

let addData calc userInput = 
    userInput :: calc.Data

let promptUserMenu () = 
    printf "You may enter a Digit or operations: +, -, /, and =, a C (for clear), and an AC (for clear all)\n"

// let getUserInput' calc input : Calculator = 
    
//     let newCalc = {calc with Data = addData calc input}

//     match input |> isOperatorOrNumberOrEqual with 
//     | true, _, _ -> {newCalc with  LastOp = opsDict.[input]}
//     | _, true, _ -> updateState {newCalc with LastNum = int input} input
//     | _, _, true -> updateState calc (string calc.LastNum)
//     | _ -> calc


let getUserInput calc input : Calculator = 
    
    let newCalc = {calc with Data = addData calc input}

    match InputType input with 
    | Numeric x -> updateState {newCalc with LastNum = int input} input
    | ArithemticOperator x -> {newCalc with  LastOp = opsDict.[input]}
    | Command x -> updateState calc (string calc.LastNum)
    | Invalid -> calc 
 

