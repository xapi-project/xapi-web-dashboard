
open Sexplib.Std

type pool_info = {
  pool_name : string;
  pool_uuid : string;
  session : string;
  version : string;
}

type connection = {
  host : string;
  username : string;
  password : string;
} with sexp

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

let remember_host host username password =
  mutate key
    (fun s ->
      { host; username; password }
      :: (List.filter (fun c -> c.host <> host) s))

let iter_hosts fn =
  Js.Opt.iter
    (st#get key)
    (fun s ->
      let list = of_string (Js.to_string s) in
      List.iter fn list)

let forget_host host =
  mutate key
    (List.filter (fun c -> c.host <> host))


open Lwt

module Client=Client.ClientF(Lwt)

let testrpc server x = 
  let result = Rpc_client_js.do_json_rpc ~url:(Printf.sprintf "http://%s/jsonrpc" server) x in
  result

let connect h =
  let rpc = testrpc h.host in
  Client.Session.login_with_password rpc h.username h.password "1.0" >>= fun session ->
  Client.Pool.get_all_records rpc session >>= fun pools ->
  let (pool_ref,pool_rec) = List.hd pools in
  let info = {
    pool_name = pool_rec.API.pool_name_label;
    pool_uuid = pool_rec.API.pool_uuid;
    session = session;
    version = "1.0";
  } in
  Lwt.return (rpc,info)


