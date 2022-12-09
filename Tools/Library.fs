namespace Tools

open System.Text.RegularExpressions
open System.Drawing
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Memory

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
    let splitJoin splitBy funcManipulateArray (str: string) = Regex.Split(str, splitBy) |> funcManipulateArray |> String.concat splitBy

module Gif =
    let charToColor char = if char = ' ' then Color.Black else Color.White

    let createImageWithPixelSeq width height pixels =
        let image = new Image<Rgba32>(width, height, Color.Black)

        for (x, y, char) in pixels do
            image[x, y] <- charToColor char
        image


    let createImageWithXYFunc width height funcGet =
        let image = new Image<Rgba32>(width, height, Color.Black)

        for x in [|0..width-1|] do
            for y in [|0..height-1|] do
                let char = funcGet x y 
                image[x, y] <- charToColor char
        image

    let createImage (data: char array array) =
        let width = Array.length data[0]
        let height = Array.length data
        let image = new Image<Rgba32>(width, height, Color.Black)

        for x in [|0..width-1|] do
            for y in [|0..height-1|] do
                let char = data[y][x]
                image[x, y] <- charToColor char
        image

    let saveAsGif (filename: string) (gif: Image) =
        gif.SaveAsGif(filename)

    let createGif (data: char array array seq) =
        let first = data |> Seq.head
        let width = Array.length first[0]
        let height = Array.length first

        let frameDelay = 100 // Delay between frames in (1/100) of a second.

        //let colors = [ Color.Green; Color.Red ]

        // Create empty image.
        use gif = new Image<Rgba32>(width, height, Color.Black)

        // Set animation loop repeat count to 5.
        let mutable gifMetaData = gif.Metadata.GetGifMetadata()
        gifMetaData.RepeatCount <- 5us

        // Set the delay until the next image is displayed.
        let mutable metadata = gif.Frames.RootFrame.Metadata.GetGifMetadata()
        metadata.FrameDelay <- frameDelay

        for frameData in data do
            use image = createImage frameData

        //let frames = colors |> List.toSeq |> Seq.map (fun clr ->
            //use image = new Image<Rgba32>(width, height, clr)
            //let buffer = new Buffer2D<Rgba32>()
            //image.Frames.RootFrame.PixelBuffer <- buffer
            // Set the delay until the next image is displayed.
            let metadata = image.Frames.RootFrame.Metadata.GetGifMetadata()
            metadata.FrameDelay <- frameDelay

            gif.Frames.AddFrame(image.Frames.RootFrame) |> ignore

        //for frame in frames do
        //    gif.Frames.AddFrame(frame) |> ignore

        gif
        // Save the final result.
        //Image.FromStream
        //Image.FromFile("myimage.png");
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