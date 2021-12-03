module PowerConsumption

type Bit =
    | One 
    | Zero

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

let gammaBit bitList =
    let count = bitList |> List.where (fun x -> x = One) |> List.length
    if count > (bitList.Length / 2) then
        One
    else
        Zero
    

