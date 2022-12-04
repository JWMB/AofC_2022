namespace Tools

open System.Text.RegularExpressions

module Parsing =
    let parseRows (input: string) rowParser = 
        Regex.Split(input.Trim().Replace("\r", ""), @"\n") 
        |> Array.map rowParser
