open Lwt

module Client=Client.ClientF(Lwt)

let testrpc server x = 
  let result = Rpc_client_js.do_json_rpc ~url:(Printf.sprintf "http://%s/jsonrpc" server) x in
  Firebug.console##log (Js.string "Got here...");
  result

let testhtml n =
  <:html<     <section class="hero">
	<div class="row">
          <div class="medium-4 medium-offset-4">
            <p>Nothing to see here $int:n$ </p>
          </div>
        </div>
      </section>
  >>

let num = ref 1
let d = Dom_html.document
let get_by_id id =
  Js.Opt.get (d##getElementById(Js.string id))
    (fun () -> assert false)

let ( >>?= ) = Js.Opt.iter

let get_val id =
  let input = get_by_id id in
  let input_node = Js.Opt.get (Dom_html.CoerceTo.input input) (fun _ -> assert false) in
  let v = input_node##value in
  Js.to_string v

let action () =
  let server = get_val "login_server" in
  let username = get_val "login_username" in
  let password = get_val "login_password" in
  Firebug.console##log (Js.string (Printf.sprintf "server: %s username: %s password: %s" server username password));
  let rpc = testrpc server in
  let _ =
    Client.Session.login_with_password rpc username password "1.0" >>= fun x ->
    Client.Event.from rpc x ["*"] "" 1.0 >>= fun res ->
    let events = Event_types.event_from_of_rpc res in
    Firebug.console##log (Js.string (Printf.sprintf "Length=%d" (List.length events.Event_types.events)));
    Client.VM.get_all_records rpc x >>= fun l ->
    Firebug.console##log (Js.string "hello");
    List.iter (fun (vm_ref,vm_rec) ->
      Firebug.console##log (Js.string (Printf.sprintf "Name: %s" vm_rec.API.vM_name_label))) l;
    Lwt.return ()
  in

  let demo =
    Js.Opt.get (d##getElementById(Js.string "demo"))
      (fun () -> assert false) in
  demo##innerHTML <- Js.string (Cow.Xml.to_string (testhtml !num));
  incr num

let onload _ =



  let demo =
    Js.Opt.get (d##getElementById(Js.string "demo"))
      (fun () -> assert false) in
  demo##innerHTML <- Js.string (Cow.Xml.to_string (testhtml 0));

  let login_button =
    Js.Opt.get (d##getElementById(Js.string "login_button"))
      (fun () -> assert false) in

  login_button##onclick <- Dom_html.handler (fun _ -> action (); Js._true);
  

  (*	Lwt.return ()*)
  Js._true

let _ =
  Dom_html.window##onload <- Dom_html.handler onload
