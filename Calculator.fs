module Calculator


type Calculator = {
    State : float
    Data: list<string>
    LastNum: float
    LastOp: float -> float -> float
    }

let opsDict = dict [
        "+" , (+); 
        "-", (-); 
        "/", (/); 
        "*", (*)]

let calMax = 99999999.
let calMin = -99999999.

type UserInput = 
    | NumericValue of string
    | ArithemticOperator of string
    | EqualOperator of string
    | Clear of string
    | OutOfBounds of string
    | Invalid 

let determineType (x : string)= 
    let xParsed = System.Double.TryParse x

    match x with 
    | x when opsDict.Keys |> Seq.contains x -> ArithemticOperator x 
    | x when xParsed |> snd > calMax || xParsed|> snd < calMin -> OutOfBounds x
    | x when xParsed |> fst  -> NumericValue x
    | x when x = "=" -> EqualOperator x 
    | x when x = "AC" -> Clear x
    | _ -> Invalid

let performCalculation calc = 
    // printfn "%A, %A, %A" calc.State calc.LastOp calc.LastNum
    calc.LastOp calc.State calc.LastNum

let updateState calc input  = 
    let divisionByZero = ("/", 0.)

    match calc.Data.Length with
    | 1 -> {calc with State = System.Math.Round((float input),2)}
    |_ -> 
        match (calc.Data.[1], float input) = divisionByZero with 
        | true-> printfn "ERR" ; { State = 0.; Data = []; LastNum = 0.; LastOp = fun x -> (fun x -> x)}
        | _ -> {calc with State = System.Math.Round((performCalculation calc),2)}

// let isOperatorOrNumberOrEqual x =
//     opsDict.Keys |> Seq.contains x,  System.Int32.TryParse x|> fst,  x = "="

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

    match determineType input with 
    | OutOfBounds x -> printfn "ERR" ; calc
    | NumericValue x -> updateState {newCalc with LastNum = float input} input
    | ArithemticOperator x -> {newCalc with  LastOp = opsDict.[input]}
    | EqualOperator x -> updateState calc (string calc.LastNum)
    | Clear x -> { State = 0.; Data = []; LastNum = 0.; LastOp = fun x -> (fun x -> x)}
    | Invalid -> calc 


