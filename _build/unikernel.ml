open Lwt
open Lwt.Infix
open Printf

let server_src = Logs.Src.create "server" ~doc:"HTTP server"
module Server_log = (val Logs.src_log server_src : Logs.LOG)

(* Transform a stream `s into a string *)
let string_of_stream stream =
  let s = List.map Cstruct.to_string stream in
  return (String.concat "" s)

module Main (C:V1_LWT.CONSOLE) (Clock:V1.CLOCK) (FS:V1_LWT.KV_RO) (S:Cohttp_lwt.Server) = struct
  module Logs_reporter = Mirage_logs.Make(Clock)

  let start console _clock fs http =
    Logs.(set_level (Some Info));
    Logs_reporter.(create () |> run) @@ fun () ->
    
    let check_pwd u p =
      Server_log.info (fun f -> f "Check password for '%s' is '%s'" u p);
      FS.read fs u 0 4096 >>= fun readpwd ->
      match readpwd with
      | `Ok pwds ->
         string_of_stream pwds >>= fun pwd ->
         Server_log.info (fun f -> f "Found password for '%s': '%s'" u (String.trim pwd));
         if String.compare (String.trim pwd) p == 0
         then return true
         else return false
      | _ -> return false
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
    let port = Key_gen.port () in
    Server_log.info (fun f -> f "listening on %d/TCP" port);
    http (`TCP port) (S.make ~conn_closed ~callback ())

end
