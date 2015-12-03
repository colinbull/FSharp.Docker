#I @"packages/FAKE/tools"
#r @"packages/FAKE/tools/FakeLib.dll"
#r @"packages/FSharp.Data/lib/net40/FSharp.Data.dll"

[<AutoOpen>]
module Docker =

    open System
    open FSharp.Data
    open Fake.ProcessHelper

    exception CommandException of string * seq<ConsoleMessage>
    
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

    type Image = {
        Repository : string
        Tag : string
        Digest : string
        ImageId : string
        Created : string
        VirtualSize : string
    }
    
    let images() =
        let (title::data) = execRequireMachine "docker" "images --digests=true -a" [] []
        let indexes =
            ["REPOSITORY"; "TAG"; "DIGEST"; "IMAGE ID"; "CREATED"; "VIRTUAL SIZE"]
            |> Seq.map (title.IndexOf)
            |> (fun x -> Seq.append x [title.Length - 1])
            |> Seq.filter ((<=) 0)
            |> Seq.pairwise
            |> Seq.toList

        printfn "%A" indexes
        let parseLine (str:string) =
            let result =  indexes |> List.map (fun (x,y) -> str.Substring(x, (min (str.Length - 1) y) - x).Trim())
            printfn "%A" result
            match result with
            | [rep; tag; digst; image; created; size] ->
                Some { Repository = rep; Digest = digst;  Tag = tag; ImageId = image; Created = created; VirtualSize = size }
            | _ -> None

        data
        |> List.choose parseLine

    let removeImage images =
        [
            for image in images do
                yield execRequireMachine "docker" (sprintf "rmi %s" image.ImageId) [] []
        ]
        
        
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
        
