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

let (|Number|Operator|EqualSign|ClearAll|OutOfBounds|Invalid|) (x: string) = 

    let xParsed = System.Double.TryParse x

    match x with 
    | x when xParsed |> snd > calMax || xParsed|> snd < calMin -> OutOfBounds 
    | x when xParsed |> fst  -> Number 
    | x when opsDict.Keys |> Seq.contains x -> Operator 
    | x when x = "=" -> EqualSign 
    | x when x = "AC" -> ClearAll 
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


let processUserInput calc input : Calculator = 
    
    let newCalc = {calc with Data = addData calc input}

    match input with
    | OutOfBounds  -> printfn "ERR"; calc
    | Number -> updateState {newCalc with LastNum = float input} input
    | Operator -> {newCalc with  LastOp = opsDict.[input]}
    | EqualSign -> updateState calc (string calc.LastNum)
    | ClearAll -> { State = 0.; Data = []; LastNum = 0.; LastOp = fun x -> (fun x -> x)}
    |  _ -> calc 

