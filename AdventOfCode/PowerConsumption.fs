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

let chooseForOxygen acc = if acc.Ones >= acc.Zeros then One else Zero
let chooseForCarbonDioxide acc = if acc.Ones < acc.Zeros then One else Zero

let chooseAll accList = 
    accList |> List.map choose

let gammaBit bitList =
    bitList
    |> List.fold calculate init
    |> choose

let gammaByChoice choice bitList =
    bitList
    |> List.fold calculate init
    |> choice

let gamma (reports : Bit List List) =
    reports
    |> List.fold calculateAll (initAll reports )
    |> chooseAll
    
let gammaFromRaw (reports : string list) =
    reports
    |> List.map decodeBits
    |> gamma

let epsilon gamma = 
    let flip bit = match bit with | One -> Zero | Zero -> One
    gamma
    |> List.map flip

let bitValue bit index =
    if bit = Zero then 0
    else
        pown 2 index

let toDecimal gamma =
    gamma
    |> List.rev
    |> List.mapi (fun i x -> bitValue x i )
    |> List.sum

let getTotal reports =
    let gamma = gammaFromRaw reports 
    let epsilon = epsilon gamma 
    (toDecimal gamma) * (toDecimal epsilon)
    
let search choice (searchList : Bit[] List) index =
    let bits = searchList |> List.map (fun x -> x.[index])
    let result = gammaByChoice choice bits
    searchList |> List.filter (fun x -> x.[index] = result)

let rec searchRec choice index (searchList : Bit[] List) =
    if searchList.Length <= 1 
    then searchList
    else
        search choice searchList index
        |> searchRec choice (index + 1)

let searchForOxygenRating reports =
    reports 
    |> List.map (decodeBits >> List.toArray)
    |> searchRec chooseForOxygen 0

let searchForCO2Rating reports =
    reports 
    |> List.map (decodeBits >> List.toArray)
    |> searchRec chooseForCarbonDioxide 0

