#load "Docker.fsx"

open Docker

Docker.machineEnvironment <- Some (Docker.Machine.env "default")

let version = version()
let logs = Docker.logs [] "sqlprov_mysql"
let history = Docker.history [] "sqlprov_mysql"

