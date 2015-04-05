
open Sexplib.Std

type conn_state =
  | Disconnected
  | Connecting
  | Connected

type connection = {
  host : string;
  username : string;
  password : string;
} with sexp

type state = {
  c : connection;
  rpc : Rpc.call -> Rpc.response Lwt.t;
  mutable st : conn_state;
  mutable session : string;
  mutable pool_ref : string;
}

let states : state list ref = ref []

type conn_list = connection list with sexp

let st = LocalStorage.init_js ()

let of_string s = conn_list_of_sexp (Sexplib.Sexp.of_string s)
let to_string l = Sexplib.Sexp.to_string (sexp_of_conn_list l)

let key = Js.string "connections"

let mutate key fn =
  let init =
    Js.Opt.case (st#get key)
      (fun () -> [])
      (fun s -> of_string (Js.to_string s))
  in
  st#set key (Js.string (to_string (fn init)))

let remember_connection host username password =
  let connection = { host; username; password; } in
  mutate key
    (fun s ->
      connection :: (List.filter (fun c -> c.host <> host) s));
  connection

let iter_connections fn =
  Js.Opt.iter
    (st#get key)
    (fun s ->
      let list = of_string (Js.to_string s) in
      List.iter fn list)

let forget_connection host =
  mutate key
    (List.filter (fun c -> c.host <> host))

let testrpc server x = 
  let result = Rpc_client_js.do_xml_rpc ~url:(Printf.sprintf "http://%s/" server) x in
  result





module Client=Client.ClientF(Lwt)

open Lwt


let mkstate conn =
  { c=conn; st=Disconnected; rpc=testrpc conn.host; session=""; pool_ref="" }

let init () =
  states := [];
  iter_connections (fun conn ->
    states := (mkstate conn) :: !states)

let add conn =
  try
    List.find (fun st -> st.c = conn) !states
  with Not_found ->
    let state = mkstate conn in
    states := state :: !states;
    state

let all_states () =
  !states

(* See js/hooks.js *)
let open_host_modal () =
  Js.Unsafe.fun_call (Js.Unsafe.variable "open_add_host") [| |]

let close_host_modal () =
  Js.Unsafe.fun_call (Js.Unsafe.variable "close_add_host") [| |]

let connect st =
  Firebug.console##log (Js.string ("connecting host " ^ st.c.host));
  Client.Session.login_with_password st.rpc st.c.username st.c.password "1.0" >>= fun session ->
  Firebug.console##log (Js.string ("got a session "));
  Client.Pool.get_all st.rpc session >>= fun pool_refs ->
  Firebug.console##log (Js.string ("got pool refs "));
  st.pool_ref <- List.hd pool_refs;
  st.session <- session;
  st.st <- Connecting;
  let th = Cache.start st.rpc st.session in
  Firebug.console##log (Js.string ("cache started " ^ st.c.host));
  Lwt.return (st, th)
  
let disconnect st =
  Client.Session.logout st.rpc st.session;
  st.session <- "";
  st.st <- Disconnected;
  Lwt.return st 

let forget st =
  forget_connection st.c.host; 
  disconnect st >>= fun st ->
  states := List.filter (fun st' -> st'.c <> st.c) !states;
  Lwt.return ()
  

