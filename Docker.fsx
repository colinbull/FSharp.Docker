#I @"packages/FAKE/tools"
#r @"packages/FAKE/tools/FakeLib.dll"
#r @"packages/FSharp.Data/lib/net40/FSharp.Data.dll"

[<AutoOpen>]
module Docker =

    open System
    open FSharp.Data
    open Fake.ProcessHelper

    [<AutoOpen>]
    module Types =

        type Image = {
            Repository : string option
            Tag : string option
            Digest : string option
            ImageId : string
            Created : DateTime
            VirtualSize : float
        }

    [<AutoOpen>]
    module Patterns =

        let endsWith pred (str:string) =
            pred |> List.exists (fun p -> str.EndsWith(p))
        
        let private removeMap rem f (inp:string) =
            let inp = rem |> List.fold (fun (s:string) x -> s.Replace(x, "").Trim()) inp
            f(inp.Trim())

        let (|MatchEndAndConvert|_|) pred f (str:string) =
            let foundMatch = endsWith pred str
            printfn "%A" str
            if foundMatch
            then Some(removeMap pred f str)
            else None
            
        let (|StringOpt|) (str:string) =
            let str = str.Trim()
            if str.ToLowerInvariant() = "<none>"
            then None
            else Some str

        let (|Date|) (str:string) =
            let str = str.Trim()
            let now = DateTime.Today.ToUniversalTime()
            match str.ToLower() with
            | MatchEndAndConvert ["hours ago"; "hour ago"] float v -> now.AddHours(-v)
            | MatchEndAndConvert ["mins ago"; "min ago"] float v -> now.AddMinutes(-v)
            | MatchEndAndConvert ["days ago"; "day ago"] float v -> now.AddDays(-v)
            | MatchEndAndConvert ["years ago"; "year ago"] int v -> now.AddYears(-v)
            | MatchEndAndConvert ["weeks ago"; "week ago"] float v -> now.AddDays(-v * 7.)
            | MatchEndAndConvert ["months ago"; "month ago"] int v -> now.AddMonths(-v)
            | s ->
                printfn "Trying to parse %A as date" s
                DateTime.Parse(s)

        let (|SplitParseFirst|_|) (splitChar:char[]) f (inp:string) =
            match inp.Split(splitChar, StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
            | value :: _ -> Some <|f value
            | _ -> None
    
    let mutable cmdTimeout = TimeSpan.FromSeconds(30.)
    let mutable workingDirectory = __SOURCE_DIRECTORY__
    let mutable machineEnvironment = None
    
    type ICommand =
         abstract GetString : unit -> string
    
    let private assertMachineEnv() =
        match machineEnvironment with
        | None -> failwith "machine environment not set, have you created a machine?"
        | Some x -> x
        
    let exec root cmd envs (options:#ICommand list) args =
        let opts = options |> List.map (fun x -> x.GetString())
        let args =
            sprintf "%s %s" cmd (String.Join(" ",opts @ args))
        let ok, messages = 
            ExecProcessRedirected (fun info ->
                envs |> List.iter (fun (n,v) -> info.EnvironmentVariables.Add(n,v))
                info.Arguments <- args
                info.FileName <- root
                info.WorkingDirectory <- workingDirectory
            ) cmdTimeout
        messages |> Seq.map (fun m -> m.Message) |> Seq.toList
        
    let execRequireMachine root cmd options args =
        let envs = assertMachineEnv()
        exec root cmd envs options args
    
    let version() =
        exec "docker" "--version" [] [] []
        
    let images() =
        let (title::data) = execRequireMachine "docker" "images --digests=true -a" [] []
        let indexes =
            ["REPOSITORY"; "TAG"; "DIGEST"; "IMAGE ID"; "CREATED"; "VIRTUAL SIZE"]
            |> Seq.map (title.IndexOf)
            |> (fun x -> Seq.append x [title.Length - 1])
            |> Seq.filter ((<=) 0)
            |> Seq.pairwise
            |> Seq.toList
        
        let parseLine (str:string) =
            let result =  indexes |> List.map (fun (x,y) -> str.Substring(x, (min (str.Length - 1) y) - x).Trim())
            printfn "%A" result
            match result with
            | [StringOpt rep; StringOpt tag; StringOpt digst; image; Date created; SplitParseFirst [|' '|] float  size] ->
                Some { Repository = rep; Digest = digst;  Tag = tag; ImageId = image; Created = created; VirtualSize = size }
            | [StringOpt rep; StringOpt tag; image; Date created; SplitParseFirst [|' '|] float  size] ->
                Some { Repository = rep; Digest = None;  Tag = tag; ImageId = image; Created = created; VirtualSize = size }
            | _ -> None

        data
        |> List.choose parseLine

    let removeImage images =
        [
            for image in images do
                yield execRequireMachine "docker" (sprintf "rmi %s" image.ImageId) [] []
        ]
        
    let removeImagesMatching f =
        images()
        |> Seq.filter (fun x -> x.Repository.IsNone && x.Tag.IsNone)
        |> removeImage
        |> List.concat   

    module Machine =

        let [<Literal>] inspectJson = "docker_machine_inspect.json"

        type Inspect = JsonProvider<inspectJson>
 
        let inspect machineName = ()

        let env name =
            let parseLine (str:string) =
                if str.StartsWith("export")
                then
                    match str.Replace("export", "").Split([|'='|], StringSplitOptions.RemoveEmptyEntries) with
                    | [|name; value|] -> Some(name.Trim(), value.Trim().Trim('"'))
                    | _ -> None
                else None
            exec "docker-machine" "env" [] [] [name]
            |> List.choose parseLine
                    
    module Build =

        type Commands =
             | NoCache
             | Remove
             | Tag of string
        with
            interface ICommand with
                member x.GetString() =
                    match x with
                    | NoCache -> "--no-cache"
                    | Remove -> "--rm"
                    | Tag tag -> sprintf "--tag %s" tag
        
        let exec (options:Commands list) dockerfile = 
            execRequireMachine "docker" "build" options [dockerfile]
            
    module Instance = 
    
        let [<Literal>] inspectJson = "docker_instance_inspect.json"

        type Inspect = JsonProvider<inspectJson>

        let inspect image = ()            
        
