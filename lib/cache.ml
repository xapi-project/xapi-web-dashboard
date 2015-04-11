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
let host_metrics = ref M.empty
let pool = ref M.empty
let message = ref M.empty

let notify = ref (fun () -> ())

let process_events pool_ref from =
  List.iter (function
    | { ty = "vm"; reference; op = `del } ->
      vm := M.remove reference !vm
    | { ty = "vm"; reference; snapshot = Some snapshot } ->
      vm := M.add reference (pool_ref, API.vM_t_of_rpc snapshot) !vm;
    | { ty = "host"; reference; op = `del } ->
      host := M.remove reference !host
    | { ty = "host"; reference; snapshot = Some snapshot } ->
      host := M.add reference (pool_ref, API.host_t_of_rpc snapshot) !host;
    | { ty = "host_metrics"; reference; op = `del } ->
      host_metrics := M.remove reference !host_metrics
    | { ty = "host_metrics"; reference; snapshot = Some snapshot } ->
      host_metrics := M.add reference (pool_ref, API.host_metrics_t_of_rpc snapshot) !host_metrics;
    | { ty = "pool"; reference; op = `del } ->
      pool := M.remove reference !pool
    | { ty = "pool"; reference; snapshot = Some snapshot } ->
      pool := M.add reference (API.pool_t_of_rpc snapshot) !pool;
    | { ty = "message"; reference; op = `del } ->
      message := M.remove reference !message
    | { ty = "message"; reference; snapshot = Some snapshot } ->
      message := M.add reference (pool_ref, API.message_t_of_rpc snapshot) !message;
    | _ -> ()
  ) from.events;
  !notify ();
  Lwt.return ()

let classes = ["VM"; "host"; "host_metrics"; "pool"]

(* Call Event.from and process the events *)
let from ?(token="") rpc session_id pool =
  Client.Event.from ~rpc ~session_id ~classes ~timeout:60. ~token
  >>= fun result ->
  let from = event_from_of_rpc result in
  process_events pool from
  >>= fun () ->
  return from

(* Old messages have to be fetched explicitly *)
let get_old_messages rpc session_id pool =
  (* XXX: should use get_since to avoid downloading 10000 records *)
  Client.Message.get_all_records rpc session_id
  >>= fun ms ->
  List.iter (fun (rf, message_t) ->
    message := M.add rf (pool, message_t) !message
  ) ms;
  return ()

(* Blocks until the initial batch of events has been processed then
   starts a background thread receiving updates forever. *)
let start rpc session_id pool =
  get_old_messages rpc session_id pool
  >>= fun () ->
  from rpc session_id pool
  >>= fun f ->
  let (_: 'a Lwt.t) =
    let rec loop f =
      from ~token:f.token rpc session_id pool
      >>= fun f ->
      loop f in
    loop f in
  return ()
