#load "Docker.fsx"

open Docker

Docker.machineEnvironment <- Some (Docker.Machine.env "default")

let version = version()
let build = Build.exec [Build.Remove; Build.Tag "sqlprov_mysql"] "docker/mysql"

let images = Docker.images()

let removeIntermediateImages() =
    Docker.images()
    |> Seq.filter (fun x -> not(x.Repository = "<none>" && x.Tag = "<none>"))
    |> Docker.removeImage
