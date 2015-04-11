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

type result = [
  | `VM of (API.ref_pool * API.ref_VM * API.vM_t)
  | `Host of (API.ref_pool * API.ref_host * API.host_t)
  | `Pool of (API.ref_pool * API.pool_t)
  | `Message of (API.ref_pool * API.ref_message * API.message_t)
]

val query: string -> result list
(** Search the known objects and return a prioritised list of possible
    matches *)
