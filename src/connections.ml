
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

let init () =
  states := [];
  iter_connections (fun conn ->
    states := { c=conn; st=Disconnected; rpc=testrpc conn.host; session=""; } :: !states)

let open_host_modal () =
  Js.Unsafe.fun_call (Js.Unsafe.variable "open_add_host") [| |]

let close_host_modal () =
  Js.Unsafe.fun_call (Js.Unsafe.variable "close_add_host") [| |]

open Lwt


module Client=Client.ClientF(Lwt)



let connect h =
  let rpc = testrpc h.host in
  Client.Session.login_with_password rpc h.username h.password "1.0" >>= fun session ->
  let _ = Cache.receive_events rpc session_id in
  Lwt.return (rpc,h)

let disconnect (rpc,info) =
  Client.Session.logout rpc info.session

  

