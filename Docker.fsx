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

        type EnvironmentVariable = {
             Name : string
             Value : string
        }
        
        type LogEntry = {
            Timestamp : DateTimeOffset
            Ordinal : int
            Level : string
            Message : string
        }
        
        type Image = {
            Repository : string option
            Tag : string option
            Digest : string option
            ImageId : string
            Created : DateTime
            VirtualSize : float
        }

        type Command = {
            WorkingDirectory : string
            Root : string
            Cmd : string
            Env: EnvironmentVariable list
            Options : string list
            Args : string list
            Timeout : TimeSpan
            RequiresMachine : bool
        }

    [<AutoOpen>]
    module internal Patterns =

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

        let (|DateTimeOffset|_|) (str:string) =
            match DateTimeOffset.TryParse(str.Trim()) with
            | true, v -> Some v
            | false, _ -> None
        
        let (|SplitParseFirst|_|) (splitChar:char[]) f (inp:string) =
            match inp.Split(splitChar, StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
            | value :: _ -> Some <|f value
            | _ -> None

    [<AutoOpen>]
    module Utils =

        let getMessage (msg:ConsoleMessage) = Some (msg.Message.Trim())

        let computeIndices (title:String) (headers:string list) =
            headers
            |> Seq.map title.IndexOf
            |> (fun x -> Seq.append x [title.Length - 1])
            |> Seq.filter ((<=) 0)
            |> Seq.pairwise
            |> Seq.toList

       
    let mutable timeout = TimeSpan.FromSeconds(30.)
    let mutable workingDirectory =  __SOURCE_DIRECTORY__
    let mutable machineEnvironment = None
       
    module Command =

        let private assertMachineEnv() =
            match machineEnvironment with
            | None -> failwith "machine environment not set, have you created a machine?"
            | Some x -> x

        let empty() =  {
            WorkingDirectory  = workingDirectory
            Root = ""
            Cmd = ""
            Env = []
            Options = []
            Args = []
            Timeout = timeout
            RequiresMachine = false
        }

        let create root cmd options args =
            { empty() with
                Root = root;
                Cmd = cmd;
                Options = options;
                Args = args;
                RequiresMachine = true  }

        let workingDirectory dir (cmd:Command) =
            { cmd with WorkingDirectory = dir }

        let noMachineEnv cmd =
            { cmd with RequiresMachine = false }
        
        let run f (cmd:Command) =
            let args =
                sprintf "%s %s" cmd.Cmd (String.Join(" ",cmd.Options @ cmd.Args))
            let envs =
                if cmd.RequiresMachine
                then
                    let es = assertMachineEnv()
                    es @ cmd.Env
                else
                    cmd.Env
            let ok, messages = 
                ExecProcessRedirected (fun info ->
                    envs |> List.iter (fun env -> info.EnvironmentVariables.Add(env.Name,env.Value))
                    info.Arguments <- args
                    info.FileName <- cmd.Root
                    info.WorkingDirectory <- cmd.WorkingDirectory
                ) cmd.Timeout
            messages
            |> Seq.choose f
            |> Seq.toList

         
    let attach options container =
        Command.create "docker" "attach" options container
        |> Command.run getMessage
    
    let build options dockerfile = 
        Command.create "docker" "build" options [dockerfile]
        |> Command.run getMessage
        
    let commit options container repository tag =
        match repository, tag with
        | "", "" | null, null ->
            Command.create "docker" "commit" options [container]
        | "", _ | null, _ ->
            failwithf "Specified a TAG arg with a null or empty REPOSITORY arg"
        | rep, "" | rep, null ->
            Command.create "docker" "commit" options [container; rep]
        | rep, tag ->
            Command.create "docker" "commit" options [container; sprintf "%s:%s" rep tag]
        |> Command.run getMessage

    let copy options container local =
        Command.create "docker" "cp" options [container;local]
        |> Command.run getMessage
    
    let create options image command args =
        Command.create "docker" "create" options [command;args]
        |> Command.run getMessage

    let diff options container =
        Command.create "docker" "diff" options [container]
        |> Command.run getMessage
        
    let exec options container command args =
        Command.create "docker" "exec" options [container; command; args]
        |> Command.run getMessage
        
    let logs options name =
        let log (msg:ConsoleMessage) =
            let str = msg.Message.Trim()
            match str.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList with
            | (DateTimeOffset date) :: value :: level :: t ->
                { Timestamp = date; Ordinal = Int32.Parse(value); Level = level; Message = (String.Join(" ", t)) }
                |> Some  
            | _ -> None  
       
        Command.create "docker" "logs" options [name]
        |> Command.run log
       

    let images options =
        match Command.create "docker" "images" options [] |> Command.run getMessage with
        | [] -> []
        | title :: data ->             
            let indexes =
                ["REPOSITORY"; "TAG"; "DIGEST"; "IMAGE ID"; "CREATED"; "VIRTUAL SIZE"]
                |> computeIndices title

            let parseLine (str:string) =
                let result =  indexes |> List.map (fun (x,y) -> str.Substring(x, (min (str.Length - 1) y) - x).Trim())
                match result with
                | [StringOpt rep; StringOpt tag; StringOpt digst; image; Date created; SplitParseFirst [|' '|] float  size] ->
                    Some { Repository = rep; Digest = digst;  Tag = tag; ImageId = image; Created = created; VirtualSize = size }
                | [StringOpt rep; StringOpt tag; image; Date created; SplitParseFirst [|' '|] float  size] ->
                    Some { Repository = rep; Digest = None;  Tag = tag; ImageId = image; Created = created; VirtualSize = size }
                | _ -> None

            data
            |> List.choose parseLine

    let removeImage options image =
         Command.create "docker" "rmi" (options @ [image]) []
         |> Command.run getMessage
         
    let removeImages options images =
        [
            for image in images do
                yield removeImage options image.ImageId
        ]

    let removeImagesMatching options f =
        images options
        |> Seq.filter f
        |> removeImages options
        |> List.concat   

    let version() =
        Command.create "docker" "--version" [] []
        |> Command.run getMessage
    
    module Machine =

        let [<Literal>] inspectJson = "docker_machine_inspect.json"
        type Inspect = JsonProvider<inspectJson>
 
        let inspect machineName = ()

        let env name =
            let parseLine (str:string) =
                if str.StartsWith("export")
                then
                    match str.Replace("export", "").Split([|'='|], StringSplitOptions.RemoveEmptyEntries) with
                    | [|name; value|] -> Some({Name = name.Trim(); Value = value.Trim().Trim('"') })
                    | _ -> None
                else None
            Command.create "docker-machine" "env" [] [name]
            |> Command.noMachineEnv
            |> Command.run (getMessage >> Option.bind parseLine)
                    
    module Instance = 
    
        let [<Literal>] inspectJson = "docker_instance_inspect.json"

        type Inspect = JsonProvider<inspectJson>

        let inspect image = ()            
        
