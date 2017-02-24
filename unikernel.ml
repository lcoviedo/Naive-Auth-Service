open Lwt
open Lwt.Infix
open Printf

(** Common signature for http and https. *)
module type HTTP = Cohttp_lwt.Server

(* Logging *)
let server_src = Logs.Src.create "server" ~doc:"HTTP server"
module Server_log = (val Logs.src_log server_src : Logs.LOG)

(* Transform a stream `s into a string *)
let string_of_stream stream =
  let s = List.map Cstruct.to_string stream in
  return (String.concat "" s)

module Main (MClock: Mirage_types.MCLOCK) (KV: Mirage_kv_lwt.RO) (S: HTTP) = struct 

  let start _clock kv http = 
    let check_pwd u p =
      Server_log.info (fun f -> f "Check password for '%s' is '%s'" u p);
      KV.read kv u 0L 4096L	>>= fun readpwd ->
      match readpwd with
			| Ok pwds ->
         string_of_stream pwds >>= fun pwd ->
         if String.compare (String.trim pwd) p == 0
         then (
					 Server_log.info (fun f -> f "Found password for '%s': '%s'" u (String.trim pwd)); 
				   return true)
				 else (
					 Server_log.info (fun f -> f "Invalid login:\n user:'%s' and password:'%s' do not match\n" u p); 
					 return false)
			| Error e ->
				(Server_log.info (fun f -> f "Error reading credentials"); return false)
    in
    let callback (_, cid) request _body =
      let uri = Cohttp.Request.uri request in
      let cid = Cohttp.Connection.to_string cid in
      Server_log.info (fun f -> f "[%s] serving %s" cid (Uri.to_string uri));
      let path = Uri.path uri in
      let user = Uri.get_query_param uri "user" in
      let pwd = Uri.get_query_param uri "password" in
      match (user,pwd) with
      | (Some u, Some p) ->
          check_pwd u p >>= fun chk ->
          if chk
          then S.respond_string ~status:`OK ~body:(sprintf "hello %s!\n" u) ()
          else S.respond_string ~status:`Unauthorized ~body:(
                 sprintf "Invalid login:\n user:'%s' and password:'%s' do not match\n" u p) ()
      | _ ->
          S.respond_string ~status:`Unauthorized ~body:"No user given\n" ()
    in  
    let conn_closed (_, cid) =
      let cid = Cohttp.Connection.to_string cid in
      Server_log.info (fun f -> f "[%s] closing" cid);
    in
    let port = Key_gen.http_port () in
    Server_log.info (fun f -> f "listening on %d/TCP" port);
    http (`TCP port) (S.make ~conn_closed ~callback ())

end