open Mirage

let stack = generic_stackv4 default_console tap0
let auth_data = generic_kv_ro "data"
let http_srv = http_server @@ conduit_direct ~tls:false stack

let port =
  let doc = Key.Arg.info ~doc:"Listening port." ["port"] in
  Key.(create "port" Arg.(opt int 8080 doc))

let main =
  let libraries = [ "re.str"; "magic-mime"; "mirage-logs" ] in
  let packages = [ "re"; "magic-mime"; "mirage-logs" ] in
  let keys = [ Key.abstract port ] in
  foreign
    ~libraries ~packages ~keys
    "Unikernel.Main" (console @-> clock @-> kv_ro @-> http @-> job)

let () =
  register "auth_service" [main $ default_console $ default_clock $ auth_data $ http_srv]
