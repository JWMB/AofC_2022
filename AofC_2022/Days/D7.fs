module D7

open Tools

type File = { name:string; size:int }

type Tree<'T> = Leaf of 'T | Branch of 'T * Tree<'T> list

let getItemFromNode = function | Branch (f, _) -> f | Leaf l -> l
let isBranch = function | Branch (_, _) -> true | Leaf _ -> false

let rec addNode parentNode newNode = function
    | Branch (value, lst) -> 
        Branch(value, 
            if (value, lst) = parentNode then lst |> List.append [newNode]
            else List.map (addNode parentNode newNode) lst
        )
    | Leaf _ as x -> x

let rec findParentByFunc funcIsNode = function
    | Branch (value, lst) ->
            if funcIsNode (value, lst) then Some((value, lst))
            else 
                let children = List.map (findParentByFunc funcIsNode) lst
                let found = children |> List.filter (fun f -> f.IsSome)
                if found.Length > 0 then found[0] else None
    | Leaf _ as x -> None

type X = { Path: string array; Content: Option<File array> }

let parseFileSystemItem str = str |> RxCurry.split "\s" |> fun f -> if f[0] = "dir" then { name = f[1]; size = 0; } else { name = f[1]; size = int f[0]; }

let getHierarchy input =
    let removeEmpty (arr: string array) = arr |> Array.filter (fun f -> f.Length > 0)

    let sections =
        Parsing.cleanWithTrim input 
        |> RxCurry.split @"\$ "
        |> removeEmpty |> Array.map (fun f -> f |> RxCurry.split "\n" |> removeEmpty)

    let cmdWithResults = sections |> Array.map(fun f -> (f |> Array.head, f |> Array.tail))

    let scanCommandsToDirectories agg curr =
        let cmdline = fst curr
        let cmdAndArgs = cmdline |> RxCurry.split " "
        match cmdAndArgs[0] with
        | "cd" ->
            match cmdAndArgs[1] with
            | "/" -> { Path = [||]; Content = None; }
            | ".." -> { Path = agg.Path |> ArrayEx.exceptLast; Content = None; } 
            | dir -> { Path = [| dir |] |> Array.append agg.Path; Content = None; }
        | "ls" ->
            { Path = agg.Path; Content = Some(snd curr |> Array.map parseFileSystemItem); }
        | _ -> { Path = [||]; Content = None; }

    let dirsAndFiles = cmdWithResults |> Array.scan scanCommandsToDirectories { Path = [||]; Content = None; }
                    |> Array.filter (fun f -> f.Content <> None)
                    |> Array.map (fun f -> (f.Path |> String.concat "/", f.Content.Value))
                    |> Array.distinctBy (fun f -> fst f)
                    |> Array.sortBy (fun f -> fst f)

    let nodes = dirsAndFiles |> Array.map (fun (name, content) -> Tree<File>.Branch({ name = name; size = 0;}, content |> Array.map (fun fn -> Tree<File>.Leaf(fn)) |> Array.toList))
        
    let hierarchy = nodes |> Array.tail 
                    |> Array.fold (fun agg curr -> 
                        let parentName = StringEx.splitJoin "/" ArrayEx.exceptLast (getItemFromNode curr).name
                        let parent = findParentByFunc (fun (f, _) -> f.name = parentName) agg
                        if parent.IsNone then failwith $"No parent found: {parentName}"
                        else addNode parent.Value curr agg) (Array.head nodes)
    hierarchy

let rec flattenChildren item = seq {
    match item with
    | Branch (_, lst) -> 
        let flattenedChildren = lst |> List.map (fun c -> (flattenChildren c) |> Seq.toList) |> List.reduce List.append
        yield! flattenedChildren

        let bboo = flattenedChildren |> List.map (fun (p, children) -> [getItemFromNode p] |> List.append children)
        yield (item, bboo |> List.reduce List.append |> List.distinct) // why is distinct needed? I'm really missing something here...
    | Leaf l -> yield (item, [])
    }

let part1 input =
    let hierarchy = getHierarchy input

    let dirsWithAllChildren = flattenChildren hierarchy |> Seq.toList |> List.filter (fun (f, _) -> isBranch f) |> List.map (fun (f, size) -> (getItemFromNode f, size))

    let sizes = dirsWithAllChildren |> List.map (fun (f, children) -> (f, children |> List.sumBy (fun f -> f.size)))
                    |> List.map (fun (_, size) -> size)
    let sum = sizes |> List.filter (fun size -> size <= 100000) |> List.sum
    sum
    
let part2 input =
    let maxSpace = 70000000
    let needed = 30000000
    let maxForExistingStructure = maxSpace - needed

    let hierarchy = getHierarchy input

    let dirsWithAllChildren = flattenChildren hierarchy |> Seq.toList |> List.filter (fun (f, _) -> isBranch f) |> List.map (fun (f, size) -> (getItemFromNode f, size))

    let sizesSorted = dirsWithAllChildren |> List.map (fun (item, children) -> (item, children |> List.sumBy (fun item -> item.size)))
                        |> List.map (fun (_, size) -> size) |> List.sort
    let total = sizesSorted |> List.max
    let overshoot = total - maxForExistingStructure

    let smallestBigNuff = sizesSorted |> List.find (fun size -> size >= overshoot)

    smallestBigNuff
