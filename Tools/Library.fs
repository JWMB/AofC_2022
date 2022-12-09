namespace Tools

open System.Text.RegularExpressions
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp

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

    let createGif (images: Image<Rgba32> seq) =
        let first = images |> Seq.head

        let frameDelay = 50 // Delay between frames in (1/100) of a second.

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
