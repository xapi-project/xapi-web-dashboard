(*
 * Copyright (C) 2012 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Lwt

open Xen_api
open Xen_api_lwt_unix

let uri = ref "http://127.0.0.1/jsonrpc"
let username = ref "root"
let password = ref "password"
let json = ref false

let exn_to_string = function
	| Api_errors.Server_error(code, params) ->
		Printf.sprintf "%s %s" code (String.concat " " params)
	| e -> Printexc.to_string e

let main () =
  let rpc = if !json then make_json !uri else make !uri in
  lwt session_id = Session.login_with_password rpc !username !password "1.0" in
  try_lwt
    Cache.start rpc session_id "pool"
    >>= fun () ->
    Printf.fprintf stderr "Type a search query and hit enter:\n%!";
    let rec loop () =
      Lwt_io.read_line Lwt_io.stdin
      >>= fun x ->
      let results = Search.query x in
      List.iteri (fun i x -> match x with
      | `VM (pool,rf, _) ->
        Printf.fprintf stderr "%d/%d: VM %s\n%!" i (List.length results) rf
      | `Host (pool,rf, _) ->
	Printf.fprintf stderr "%d/%d: Host %s\n%!" i (List.length results) rf
      | `Pool (rf, _) ->
	Printf.fprintf stderr "%d/%d: Pool %s\n%!" i (List.length results) rf
	    | `Message (pool, rf, _) ->
	Printf.fprintf stderr "%d/%d: Message %s\n%!" i (List.length results) rf
      ) results;
      loop () in
    loop ()
  finally
    Session.logout rpc session_id

let _ =
	Arg.parse [
		"-uri", Arg.Set_string uri, (Printf.sprintf "URI of server to connect to (default %s)" !uri);
		"-u", Arg.Set_string username, (Printf.sprintf "Username to log in with (default %s)" !username);
		"-pw", Arg.Set_string password, (Printf.sprintf "Password to log in with (default %s)" !password);
		"-j", Arg.Set json, (Printf.sprintf "Use jsonrpc rather than xmlrpc (default %b)" !json);
	] (fun x -> Printf.fprintf stderr "Ignoring argument: %s\n" x)
		"Simple example which lists VMs found on a pool";

	Lwt_main.run (main ())
