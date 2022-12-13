module D13

open Tools

type Nested =
    | Number of int
    | Child of Nested array

type OOO = Nested array

let parseRow (row:string) = 
    // [1,[1,2],2]
    // [[1],[2,3,4]]
    //let rec loop2 (str: string) index = seq {
    //    if index < str.Length then
    //        let char = str[index]
    //        if char = '[' then
    //            let values = loop2 str (index + 1) (index + 1)

    //            yield Child (values |> Seq.toArray)
    //        elif (char = ']' || char = ',') then
    //            //if startIndex < index then
    //            //    yield Number (int (str.Substring(startIndex, index-startIndex)))
    //            //if char = ',' then
    //            //    yield! loop str (index + 1) (index + 1)
    //            // TODO: what when ] ?
    //            //else
                    
    //        else // start of a number
                
    //            //yield! loop str startIndex (index + 1)
    //    }

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
                    //yield (lastIndex, Some(Child (values |> Array.map (fun (_, v) -> v))))
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

    //let rec loop (str: string) startIndex index = seq {
    //    if index < str.Length then
    //        let char = str[index]
    //        if char = '[' then
    //            yield Child (loop str (index + 1) (index + 1) |> Seq.toArray)
    //        elif (char = ']' || char = ',') then
    //            if startIndex < index then
    //                yield Number (int (str.Substring(startIndex, index-startIndex)))
    //            if char = ',' then
    //                yield! loop str (index + 1) (index + 1)
    //            // TODO: what when ] ?
    //            //else
                    
    //        else // start of a number
    //            yield! loop str startIndex (index + 1)
    //    }

    let xx = loop (row.Substring(1, row.Length - 2)) 0 0 //.Trim("]", "")
    xx |> Seq.toArray |> Array.map (fun (_, v) -> v)  |> getSome

let toString nestedArray =
    let rec loop node = seq {
        match node with
        | Number num -> yield $"{num}"
        | Child chi -> 
            let ii = (chi |> Array.map loop |> Array.map (String.concat ",")) |> String.concat ","
            yield $"[{ii}]"
        //yield $"[{((chi |> Array.map loop) |> String.concat ",")}]"
        }
    loop (Child nestedArray) |> String.concat ""

let parseSection section =
    let rows =RxCurry.splitTrimNoEmpty "\n" section |> Array.map parseRow
    (rows[0], rows[1])


let part1 input =
    //let t1 = parseRow "[[1],[2,3,4]]"
    //let t1 = parseRow "[[]]"
    //let t1s = toString t1 
       
    let pairs = input |> Parsing.cleanWithTrimEmptyLines |> RxCurry.splitTrimNoEmpty @"\n\n" |> Array.map parseSection
    //let oppo = pairs |> Array.map (fun (a, _) -> toString a)

    let result = 0
    result
    
let part2 input =
    let rows = Parsing.parseRows input parseRow
    let result = 0
    result
