module D13

open Tools

type Nested =
    | Number of int
    | Child of Nested array

type OOO = Nested array

let parseRow (row:string) = 
    // [1,[1,2],2]
    // [[1],[2,3,4]]
    let getSome (arr: Option<Nested> array) = arr |> Array.filter (fun v -> v.IsSome) |> Array.map (fun v -> v.Value)
    
    let rec loop (str: string) startIndex index = seq {
        let getNumber i1 i2 = Number (int (str.Substring(i1, i2-i1)))
        if index < str.Length then
            let char = str[index]
            if char = '[' then
                let indicesAndValues = loop str (index + 1) (index + 1) |> Seq.toArray
                let lastIndex = indicesAndValues |> Array.map (fun (i, _) -> i) |> Array.max

                let valuesWithSome = indicesAndValues |> Array.map (fun (_, (v: Option<Nested>)) -> v) |> getSome
                yield (lastIndex, Some(Child (valuesWithSome)))
                yield! loop str lastIndex lastIndex

            elif (char = ']' || char = ',') then
                if startIndex < index then
                    yield (index + 1, Some(getNumber startIndex index))
                if char = ',' then
                    yield! loop str (index + 1) (index + 1)
                else // ]
                    yield (index + 1, None)
            else // start of a number
                yield! loop str startIndex (index + 1)
        else if startIndex < index then
            yield (index, Some(getNumber startIndex index))
        }

    let rawResult = loop (row.Substring(1, row.Length - 2)) 0 0 //.Trim("]", "")
    rawResult |> Seq.toArray |> Array.map (fun (_, v) -> v)  |> getSome

let toString nestedArray =
    let rec loop node = seq {
        match node with
        | Number num -> yield $"{num}"
        | Child chi -> 
            let ii = (chi |> Array.map loop |> Array.map (String.concat ",")) |> String.concat ","
            yield $"[{ii}]"
        }
    loop (Child nestedArray) |> String.concat ""

let rec getAsNumberSequence nested = seq {
        match nested with 
        | Number n ->
            yield n
        | Child c ->
            let rest = c |> Array.map getAsNumberSequence // |> Seq.reduce Seq.append
            if rest.Length > 0 then
                yield! (rest |> Seq.reduce Seq.append)
    }

let parseSection section =
    let rows =RxCurry.splitTrimNoEmpty "\n" section |> Array.map parseRow
    (rows[0], rows[1])


let part1 input =
    //let t1 = parseRow "[[1],[2,3,4]]"
    //let t1 = parseRow "[[]]"
    //let t1s = toString t1 
       
    let isFirstLargest ((a: Nested array), (b: Nested array)) =
        //let getAsNumber x = match x with | Number n -> Some(n) | _ -> None
        //let getAsChildren x = match x with | Child n -> Some(n) | _ -> None

        let rec loop (a: Nested) (b: Nested) =
            match a with
            | Number aN ->
                match b with
                | Number bN ->
                    if aN < bN then Some(true)
                    elif aN > bN then Some(false)
                    else None
                | Child bC -> loop (Child [|a|]) b
            | Child aC ->
                match b with
                | Number bN -> loop a (Child [|b|])
                | Child bC ->
                    let zipped = Seq.zip aC bC
                    let mapped = zipped |> Seq.map (fun (l, r) -> loop l r)
                    
                    let found = mapped |> Seq.tryFind (fun v -> v.IsSome)
                    if found.IsSome then found.Value
                    else 
                        if aC.Length = bC.Length then None
                        else Some(aC.Length < bC.Length)
                    
        let result = loop (Child a) (Child b)
        if result.IsNone then failwith "Equal"
        result.Value

    //        let aAsNumber = getAsNumber a
    //        let bAsNumber = getAsNumber b
    //        if aAsNumber.IsSome && bAsNumber.IsSome then
    //            if aAsNumber.Value < bAsNumber.Value then Some(true)
    //            elif aAsNumber.Value > bAsNumber.Value then Some(false)
    //            else None
    //        elif aAsNumber.IsNone && bAsNumber.IsNone then
    //            loop 
    //        else
    //            false

    //let isFirstLargest (a, b) =
    //    let sA = getAsNumberSequence (Child a)
    //    let sB = getAsNumberSequence (Child b)

    //    let comparison (l: int) (r: int) =
    //        if l < r then Some(true)
    //        elif l > r then Some(false)
    //        else None

    //    let zipped = Seq.zip sA sB
    //    let mapped = zipped |> Seq.map (fun (a, b) -> comparison a b)
    //    let found = mapped |> Seq.tryFind (fun v -> v.IsSome)
    //    if found.IsSome then found.Value.Value
    //    else (sA |> Seq.length) < (sB |> Seq.length)

    let pairs = input |> Parsing.cleanWithTrimEmptyLines |> RxCurry.splitTrimNoEmpty @"\n\n" |> Array.map parseSection
    //let oppo = pairs |> Array.map (fun (a, _) -> toString a)
    let oppo = pairs |> Array.map (fun (a, _) -> getAsNumberSequence (Child a) |> Seq.toArray)

    let indicesOfCorrect = pairs |> Array.map isFirstLargest |> Array.indexed |> Array.filter (fun (_, v) -> v) |> Array.map (fun (i, _) -> i)

    let result = indicesOfCorrect |> Array.map (fun f -> f + 1) |> Array.sum
    result // 5478 too low
    
let part2 input =
    //let rows = Parsing.parseRows input parseRow
    let result = 0
    result
