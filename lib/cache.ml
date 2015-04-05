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

open Event_types

module M = Map.Make(String)

module Client=Client.ClientF(Lwt)

type rpc = Rpc.call -> Rpc.response Lwt.t
type session_id = string

let vm = ref M.empty
let host = ref M.empty
let pool = ref M.empty

let process_events from =
  List.iter (function
    | { ty = "vm"; reference; op = `del } ->
      vm := M.remove reference !vm
    | { ty = "vm"; reference; snapshot = Some snapshot } ->
      vm := M.add reference (API.vM_t_of_rpc snapshot) !vm;
    | { ty = "host"; reference; op = `del } ->
      host := M.remove reference !host
    | { ty = "host"; reference; snapshot = Some snapshot } ->
      host := M.add reference (API.host_t_of_rpc snapshot) !host;
    | { ty = "pool"; reference; op = `del } ->
      pool := M.remove reference !pool
    | { ty = "pool"; reference; snapshot = Some snapshot } ->
      pool := M.add reference (API.pool_t_of_rpc snapshot) !pool;
    | _ -> ()
  ) from.events;
  Lwt.return ()

(* Call Event.from and process the events *)
let from ?(token="") rpc session_id =
  Client.Event.from ~rpc ~session_id ~classes:["VM"; "host"; "pool"] ~timeout:60. ~token
  >>= fun result ->
  let from = event_from_of_rpc result in
  process_events from
  >>= fun () ->
  return from

(* Blocks until the initial batch of events has been processed then
   starts a background thread receiving updates forever. *)
let start rpc session_id =
  from rpc session_id
  >>= fun f ->
  let (_: 'a Lwt.t) =
    let rec loop f =
      from ~token:f.token rpc session_id
      >>= fun f ->
      loop f in
    loop f in
  return ()
