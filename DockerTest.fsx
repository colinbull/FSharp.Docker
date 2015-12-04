#load "Docker.fsx"

open Docker

Docker.machineEnvironment <- Some (Docker.Machine.env "default")

let version = version()
let build = Build.exec [Build.Remove; Build.Tag "sqlprov_mysql"] "docker/mysql"

let images = Docker.images()
   
