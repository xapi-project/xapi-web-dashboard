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

module M : Map.S with type key = string

val vm: API.vM_t M.t ref
val host: API.host_t M.t ref

type rpc = Rpc.call -> Rpc.response Lwt.t

type session_id = string

val receive_events: ?token:string -> rpc -> session_id -> 'a Lwt.t
(** Receive events forever, populating the local cache *)
