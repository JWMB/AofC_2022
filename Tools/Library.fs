namespace Tools

open System.Text.RegularExpressions

module RxCurry =
    let matches pattern input = Regex.Matches(input, pattern)
    let split pattern input = Regex.Split(input, pattern)

module Parsing =
    let clean (input: string) = input.Replace("\r", "").Trim()

    //let splitBy pattern input = Regex.Split(input, pattern)

    let parseRows (input: string) rowParser = 
        input |> clean |> RxCurry.split "\n" |> Array.map rowParser
        //Regex.Split(input |> clean, "\n") |> Array.map rowParser
