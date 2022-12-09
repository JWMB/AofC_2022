open System
open System.Text.RegularExpressions
open System.IO
//open Tools
open System.Diagnostics

let getDayPartMethods (type_: Type) =
    type_.GetMethods() |> Seq.filter (fun m -> m.Name.StartsWith("part")) |> Seq.toArray

let findFileInParents leaf fGetFile =
    let rec loop (dir: DirectoryInfo) =
        let (foundFile: 'a option) = fGetFile dir
        if foundFile.IsNone then 
            if dir.Parent = null then None
            else loop dir.Parent
        else foundFile
    loop leaf

let findFileInParents2 filename =
    let currDir = new DirectoryInfo(Directory.GetCurrentDirectory())
    findFileInParents currDir (fun dir -> dir.GetFiles(filename) |> Array.tryHead)

let slnDir = (findFileInParents2 "AofC_2022.sln").Value.Directory
let projDir = slnDir.GetDirectories("AofC_2022").[0]

let getTypeFilePath (type_: Type) suffix = 
    let dir = projDir.GetDirectories("Days")[0];
    let files = dir.GetFiles($"{type_.Name}{suffix}")
    if files.Length = 1 then Some(files.[0]) else None

let getFileContent (type_: Type) extension = match getTypeFilePath type_ extension with
                                                | Some fi -> File.ReadAllText(fi.FullName)
                                                | None -> ""

let getDayTypes =
    let assembly = AppDomain.CurrentDomain.GetAssemblies() |> Array.find (fun a -> a.GetName().Name.StartsWith("AofC"))
    let found = assembly.ExportedTypes 
                |> Seq.map (fun t -> (t, Regex.Match(t.Name, @"^D(\d+)$"))) 
                |> Seq.filter (fun f -> (snd f).Success)
                |> Seq.map (fun f -> (int (snd f).Groups.[1].Value, fst f))
    Map(found |> Seq.toArray)

let generateReadMe (type_: Type) (aofcInfo: Tools.AofCSiteInfo.DayInfo) (basePath: DirectoryInfo) =
    let relativePath (filepath: string) = System.IO.Path.Combine(basePath.FullName, filepath).Substring(slnDir.FullName.Length).Replace(Path.DirectorySeparatorChar, '/')

    let getSnippet (method: Reflection.MemberInfo) = 
        let fsContent = getFileContent method.DeclaringType ".fs"
        let m = Regex.Match(fsContent + "\nlet ", $@"(?<=\r|\n)let {method.Name}.+?(?=(\r|\n)let\s)", RegexOptions.Singleline)
        if m.Success then m.Value.Trim()
        else ""

    let createMethodSummary input (method: Reflection.MethodInfo) = 
        let stopWatch = new Stopwatch();
        stopWatch.Start()
        let methodResult = method.Invoke(null, [|input|]).ToString()
        stopWatch.Stop()
        let elapsed = int stopWatch.Elapsed.TotalMilliseconds

        let visualizationFile = getTypeFilePath type_ $"{method.Name}.gif"

        let methodResultString = 
            if methodResult.Contains("\n") then
                $"\n```\n{methodResult}\n```"
            else $"`{methodResult}`"
        $"""### {method.Name}
```FSharp
{getSnippet method}
```
{if visualizationFile.IsSome then $"![visualization]({relativePath visualizationFile.Value.Name})  " else ""}
Result (in `{elapsed}`ms): {methodResultString}"""

    let input = getFileContent type_ ".txt"
    $"""## [Day {aofcInfo.Day} - {aofcInfo.Title}]({aofcInfo.Url})
[Source]({relativePath type_.Name + ".fs"}) | [Input]({match getTypeFilePath type_ ".txt" with | Some f -> relativePath f.Name | None -> ""})  
{getDayPartMethods type_ |> Array.map (fun f-> createMethodSummary input f) |> String.concat "\n"}
"""

let writeReadme readmeFilename =
    let readmeFile = findFileInParents2 readmeFilename
    if readmeFile.IsSome then
        let readmeContent = File.ReadAllText(readmeFile.Value.FullName)
        let startMatch = Regex.Match(readmeContent, "##Autogenerated##")
        if startMatch.Success then
            let dayTypes = getDayTypes
            let infos = dayTypes.Keys |> Seq.toArray |> Array.map (fun day -> Tools.AofCSiteInfo.main 2022 day) |> Async.Parallel |> Async.RunSynchronously
            let infoByDay = Map(infos |> Array.map(fun f -> (f.Day, f)))

            let dir = match findFileInParents2 "Program.fs" with
                        | Some v -> v.Directory
                        | None -> new DirectoryInfo(Path.Join(Directory.GetCurrentDirectory(), "AofC_2022"))
            
            let daysDir = dir.GetDirectories("Days")[0];

            let autogenerated = dayTypes |> Map.toArray 
                                         |> Array.map (fun (day, type_) ->
                                                let info = infoByDay.[day]
                                                generateReadMe type_ info daysDir
                                            ) |> String.concat ""

            let newContent = readmeContent.Substring(0, (startMatch.Index + startMatch.Length)) + "\n" + autogenerated
            File.WriteAllText(readmeFile.Value.FullName, newContent)

let getNextDay =
    let currentMax = Seq.max getDayTypes.Keys
    currentMax + 1

let addDayTest day =
    let currDir = new DirectoryInfo(Directory.GetCurrentDirectory())
    let testDir = currDir.Parent.GetDirectories("*.Tests")[0]
    if testDir.Exists then
        let testFile = testDir.GetFiles("Tests.fs")[0]
        if testFile.Exists then
            let dayTypeName = $"D{day}"
            let content = File.ReadAllText(testFile.FullName)
            if content.Contains($"``{dayTypeName}``") = false then
                let templateTests = Regex.Split(content, Regex.Escape("[<Fact>]")) |> Array.filter (fun f -> f.Contains("template"))
                if templateTests.Length = 1 then
                    let test = Regex.Replace(templateTests[0], "template", dayTypeName, RegexOptions.IgnoreCase)
                    let updatedContent = $"{content}\n\n[<Fact>]{test}"
                    File.WriteAllText(testFile.FullName, updatedContent)
                    //printfn $"{updatedContent}"

let createDayFile day =
    printfn $"creating day {day}"
    let templateName = "template.fs";
    let template = findFileInParents2 $"Days/{templateName}"
    if template.IsNone then
        printfn $"template not found"
    else
        let dNext = $"D{day}"
        let content = File.ReadAllText(template.Value.FullName).Replace("module Template", $"module {dNext}")
        let codeFilepath = template.Value.FullName.Replace(templateName, $"{dNext}.fs")
        if File.Exists(codeFilepath) = false then File.WriteAllText(codeFilepath, content)
        //printfn $"content: {content}"
        let inputFilepath = template.Value.FullName.Replace(templateName, $"{dNext}.txt")
        if File.Exists(inputFilepath) = false then File.WriteAllText(inputFilepath, "")


[<EntryPoint>]
let main argv =
    writeReadme "README.md"
    if argv.Length > 0 then
        let arg = argv[0]
        match arg with
        | "generate_readme" ->
            printfn "Generating README.md"
            writeReadme "README.md"
        | "create_nextday" ->
            let next = getNextDay
            createDayFile next
            addDayTest next
        | _ ->
            printfn $"Command not found {arg}"
    else
        let method = D9.part2
        let day = 9
        // TODO: how to figure out which day method corresponds to? D4.part2.GetType() returns a local runtime type, not associated with the target
        // Roslyn would work but seems overkill
        let dayType = getDayTypes[day]

        let input = match getTypeFilePath dayType ".txt" with
                    | Some fi -> File.ReadAllText(fi.FullName)
                    | None -> ""
        
        method input |> ignore
    0