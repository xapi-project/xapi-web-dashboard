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

type result = [
  | `VM of (API.ref_pool * API.ref_VM * API.vM_t)
  | `Host of (API.ref_pool * API.ref_host * API.host_t)
  | `Pool of (API.ref_pool * API.pool_t)
]

let contains r x =
  try
    let (_: int) = Re_str.search_forward r x 0 in
    true
  with Not_found ->
    false

let query x =
  let r = Re_str.regexp_string x in
  let vm = !Cache.vm in
  let vms = Cache.M.fold (fun rf (pool_ref, vm_t) acc ->
    if String.lowercase x = "class:vm"
    || (contains r vm_t.API.vM_name_label)
    then `VM (pool_ref, rf, vm_t) :: acc
    else acc
  ) vm [] in
  let host = !Cache.host in
  let hosts = Cache.M.fold (fun rf (pool_ref, host_t) acc ->
    if String.lowercase x = "class:host"
    || (contains r host_t.API.host_name_label)
    then `Host (pool_ref, rf, host_t) :: acc
    else acc
    ) host [] in
  let pool = !Cache.pool in
  let pools = Cache.M.fold (fun rf pool_t acc ->
    if String.lowercase x = "class:pool"
    || (contains r pool_t.API.pool_name_label)
    then `Pool (rf, pool_t) :: acc
    else acc
  ) pool [] in
  vms @ hosts @ pools
