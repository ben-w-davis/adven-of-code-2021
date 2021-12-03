module PowerConsumption

type Bit =
    | One 
    | Zero

type BitAccumulator = {
    Ones : int
    Zeros: int }

let init = { Ones = 0; Zeros = 0; }
let initAll (reports : Bit List List) =
    let head = reports |> List.head
    head |> List.map (fun x -> init)
let decodeOneBit bit =
    match bit with
    | '1' -> One
    | '0' -> Zero
    | _ -> failwith "Invalid bit"

let decodeBits sample =
    let temp =
        sample
        |> Seq.map decodeOneBit
        |> Seq.toList
    temp

let calculate acc bit =
    match bit with
    | One -> { acc with Ones = acc.Ones + 1 }
    | Zero -> { acc with Zeros = acc.Zeros + 1 }

let calculateAll (accList : BitAccumulator list) (report : Bit List) =
    report
    |> List.zip accList
    |> List.map (fun (acc, bit) -> calculate acc bit)

let choose acc = 
    if acc.Ones > acc.Zeros then One else Zero

let chooseAll accList = 
    accList |> List.map choose

let gammaBit bitList =
    bitList
    |> List.fold calculate init
    |> choose

let gamma (reports : Bit List List) =
    reports
    |> List.fold calculateAll (initAll reports )
    |> chooseAll
    
    //[Zero;Zero;One;Zero;Zero]
    

