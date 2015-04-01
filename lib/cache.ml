(*
 * Copyright (C) 2015 Citrix Systems Inc.
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

module M = Map.Make(String)

type rpc = Rpc.call -> Rpc.response Lwt.t
type session_id = string

let vm = ref M.empty
let host = ref M.empty

let rec receive_events ?(token="") rpc session_id =
  Event.from ~rpc ~session_id ~classes:["VM"; "host"] ~timeout:60. ~token
  >>= fun result ->
  let open Event_types in
  let from = event_from_of_rpc result in
  List.iter (function
    | { ty = "vm"; reference; op = `del } ->
      vm := M.remove reference !vm
    | { ty = "vm"; reference; snapshot = Some snapshot } ->
      vm := M.add reference (API.vM_t_of_rpc snapshot) !vm;
    | { ty = "host"; reference; op = `del } ->
      host := M.remove reference !host
    | { ty = "host"; reference; snapshot = Some snapshot } ->
      host := M.add reference (API.host_t_of_rpc snapshot) !host;
    | _ -> ()
  ) from.events;
  receive_events ~token:from.token rpc session_id
