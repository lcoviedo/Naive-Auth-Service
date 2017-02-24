open Mirage

let stack = generic_stackv4 default_network
let http_srv = http_server @@ conduit_direct ~tls:false stack
let auth_data = generic_kv_ro "data"

let http_port =
  let doc = Key.Arg.info ~doc:"Listening HTTP port." ["http"] in
  Key.(create "http_port" Arg.(opt int 8080 doc))
	
let main = 
	let packages = [
		package "re"; package "magic-mime"; package "mirage-logs"
  ] in
	let keys = [ Key.abstract http_port ] in
	foreign
	  ~packages ~keys
		"Unikernel.Main" (mclock @-> kv_ro @-> http @-> job)
		
let () =
	register "auth_service" [main $ default_monotonic_clock $ auth_data $ http_srv]