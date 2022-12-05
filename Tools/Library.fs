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
