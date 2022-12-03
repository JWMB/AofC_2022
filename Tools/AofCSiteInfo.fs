namespace Tools

open System.Net.Http
open System.Text.RegularExpressions

type HttpStatic private () =
    static let _client = new HttpClient()
    static member Client = _client


module AofCSiteInfo =
    type DayInfo = { Day: int; Title: string; Url: string; }

    let main year day =
        let url = $"https://adventofcode.com/{year}/day/{day}"
        async {
            let! response = HttpStatic.Client.GetStringAsync(url) |> Async.AwaitTask
            let m = Regex.Match(response, @": (.+?)?(?=\s*-{2,})")
            let result = {
                Day = day;
                Title = if m.Success then m.Groups.[1].Value else "N/A";
                Url = url;
            }
            return result
        }
