namespace Tools

open System.Text.RegularExpressions
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp

module RxCurry =
    let matches pattern input = Regex.Matches(input, pattern)
    let split pattern input = Regex.Split(input, pattern)
    let splitTrimNoEmpty pattern input = 
        Regex.Split(input, pattern) 
        |> Array.map (fun f -> f.Trim())
        |> Array.filter (fun f -> f.Length > 0)

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

module Geometry =
    type Vector2D = { x: int; y: int; } with
        static member empty = { x = 0; y = 0; }
        //TODO: how? static member (+) pt1 pt2 = pt1.add pt2
        member this.mul scalar = { x = this.x * scalar; y = this.y * scalar; }
        member this.add pt = { x = this.x + pt.x; y = this.y + pt.y; }
        member this.sub pt = { x = this.x - pt.x; y = this.y - pt.y; }
        member this.maxAbs = max (abs this.x) (abs this.y)
        member this.withSideMaxLength l =
            let normalize v = if v = 0 then 0 else v / abs v
            { x = l * (normalize this.x); y = l * (normalize this.y); }

    type Rect = { topLeft: Vector2D; size: Vector2D } with
        static member empty = { topLeft = Vector2D.empty; size = Vector2D.empty;  }
        static member getBoundingRect positions = positions |> Seq.tail |> Seq.fold (fun (agg: Rect) curr -> agg.expand curr) (Rect.empty.move (positions |> Seq.head))

        member this.left = this.topLeft.x
        member this.right = this.topLeft.x + this.size.x
        member this.top = this.topLeft.y
        member this.bottom = this.topLeft.y + this.size.y
        member this.width = this.size.x
        member this.height = this.size.y

        member this.move pt = { topLeft = this.topLeft.add pt; size = this.size; }
        member this.normalize = { topLeft = Vector2D.empty; size = this.size; } // TODO: normalize negative size?

        member this.expand pt = 
            let expand (value, length) newVal =
                if newVal < value then (newVal, (value - newVal) + length) 
                elif newVal > (value + length) then (value, newVal - value)
                else (value, length)
            let leftWidth = expand (this.topLeft.x, this.size.x) pt.x
            let topHeight = expand (this.topLeft.y, this.size.y) pt.y
            { topLeft = { x = fst leftWidth; y = fst topHeight}; size = { x = snd leftWidth; y = snd topHeight; }}

        member this.contains pt =
            pt.x >= this.left && pt.y >= this.top && pt.x <= this.right && pt.y <= this.bottom

module Gif =
    let charToColor char = if char = ' ' then Color.Black else Color.White

    let createImageWithPixelSeq width height pixels =
        let image = new Image<Rgba32>(width, height, Color.Black)
        for (x, y, char) in pixels do
            image[x, y] <- charToColor char
        image

    //let createImageWithXYFunc width height funcGet =
    //    let image = new Image<Rgba32>(width, height, Color.Black)
    //    for x in [|0..width-1|] do
    //        for y in [|0..height-1|] do
    //            let char = funcGet x y 
    //            image[x, y] <- charToColor char
    //    image

    //let createImage (data: char array array) =
    //    let width = Array.length data[0]
    //    let height = Array.length data

    //    let image = new Image<Rgba32>(width, height, Color.Black)

    //    for x in [|0..width-1|] do
    //        for y in [|0..height-1|] do
    //            let char = data[y][x]
    //            image[x, y] <- charToColor char
    //    image

    let saveAsGif (filename: string) (gif: Image) =
        gif.SaveAsGif(filename)

    let createGif frameDelay (images: Image<Rgba32> seq) =
        let first = images |> Seq.head

        //let frameDelay = 50 // Delay between frames in (1/100) of a second.

        let gif = first //new Image<Rgba32>(width, height, Color.Black)

        let mutable gifMetaData = gif.Metadata.GetGifMetadata()
        gifMetaData.RepeatCount <- 0us

        // Set the delay until the next image is displayed.
        let mutable metadata = gif.Frames.RootFrame.Metadata.GetGifMetadata()
        metadata.FrameDelay <- frameDelay

        for image in (images |> Seq.tail) do
            // Set the delay until the next image is displayed.
            let metadata = image.Frames.RootFrame.Metadata.GetGifMetadata()
            metadata.FrameDelay <- frameDelay 
            // TODO: add delay on last? But how can we tell it's the last without iterating further (increase memory load)?
            // lazy evaluation of actual image?

            gif.Frames.AddFrame(image.Frames.RootFrame) |> ignore

        gif
