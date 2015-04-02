
open Sexplib.Std

type conn_state =
  | Disconnected
  | Connecting
  | Connected

type connection = {
  host : string;
  username : string;
  password : string;
  mutable state : state;
  mutable session : string;
} with sexp

type state = {
  c : connection;
  st : conn_state;
  rpc : Rpc.call -> Rpc.response;
  mutable session : string;
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
  let connection = { host; username; password; state=Disconnected; session="" } in
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
  let result = Rpc_client_js.do_json_rpc ~url:(Printf.sprintf "http://%s/jsonrpc" server) x in
  result





module Client=Client.ClientF(Lwt)

open Lwt


let mkstate conn =
  { c=conn; st=Disconnected; rpc=testrpc conn.host; session=""; }

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
  Client.Session.login_with_password st.rpc st.c.username st.c.password "1.0" >>= fun session ->
  st.session <- session;
  st.state <- Connecting;
  let _ = Cache.receive_events rpc session_id in
  Lwt.return (rpc,h)

let disconnect st =
  Client.Session.logout st.rpc st.session;
  states 

  

