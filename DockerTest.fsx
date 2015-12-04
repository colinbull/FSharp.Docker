#load "Docker.fsx"

open Docker

Docker.machineEnvironment <- Some (Docker.Machine.env "default")

let version = version()
let logs = Docker.logs [] "sqlprov_mysql"
   
