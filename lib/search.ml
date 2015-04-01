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
  | `VM of (string * API.vM_t)
]

let query x =
  let r = Re_str.regexp_string x in
  let vm = !Cache.vm in
  Cache.M.fold (fun rf vm_t acc ->
    try
      let (_: int) = Re_str.search_forward r vm_t.API.vM_name_label 0 in
      `VM (rf, vm_t) :: acc
    with Not_found ->
      acc
  ) vm []
