namespace Tools

open System.Text.RegularExpressions

module RxCurry =
    let matches pattern input = Regex.Matches(input, pattern)
    let split pattern input = Regex.Split(input, pattern)

module Parsing =
    let cleanWithTrimEmptyLines (input: string) = input.Replace("\r", "").Trim('\n')
    let cleanWithTrim (input: string) = input.Replace("\r", "").Trim()

    let parseRows (input: string) rowParser = 
        input |> cleanWithTrim |> RxCurry.split "\n" |> Array.map rowParser

module ArrayEx =
    let exceptLast arr = arr |> Array.rev |> Array.tail |> Array.rev

module StringEx =
    let join str1 str2 = String.concat "" [| str1; str2; |]
    //let joinAll strs = String.concat "" strs
    let splitJoin splitBy funcManipulateArray (str: string) = Regex.Split(str, splitBy) |> funcManipulateArray |> String.concat splitBy

//type Tree<'LeafData,'INodeData> =
//    | LeafNode of 'LeafData
//    | InternalNode of 'INodeData * Tree<'LeafData,'INodeData> seq

//module Tree =
//    let rec cata fLeaf fNode (tree:Tree<'LeafData,'INodeData>) :'r =
//        let recurse = cata fLeaf fNode
//        match tree with
//        | LeafNode leafInfo ->
//            fLeaf leafInfo
//        | InternalNode (nodeInfo,subtrees) ->
//            fNode nodeInfo (subtrees |> Seq.map recurse)

//    let rec fold fLeaf fNode acc (tree:Tree<'LeafData,'INodeData>) :'r =
//        let recurse = fold fLeaf fNode
//        match tree with
//        | LeafNode leafInfo ->
//            fLeaf acc leafInfo
//        | InternalNode (nodeInfo,subtrees) ->
//            // determine the local accumulator at this level
//            let localAccum = fNode acc nodeInfo
//            // thread the local accumulator through all the subitems using Seq.fold
//            let finalAccum = subtrees |> Seq.fold recurse localAccum
//            // ... and return it
//            finalAccum

//    let rec replaceNode node newNode = function
//    | x when x = node -> newNode
//    | InternalNode (node, sub) -> List.map (replaceNode node newNode) node |> InternalNode
//    | LeafNode _ as x -> x

    //let rec containsNode node = function
    //| LeafNode _ as x -> x = node
    //| InternalNode (l, sub) as b -> b = node || List.exists (containsNode node) l

    //let upsertNode tree nodeSelector newNode =
    //    let rec xxx node =
    //        node 
    //    tree