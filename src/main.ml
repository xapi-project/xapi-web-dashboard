
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

open Lwt

let action () =
  let server = get_val "login_server" in
  let username = get_val "login_username" in
  let password = get_val "login_password" in
  let conn = Connections.remember_connection server username password in
  Connections.connect (Connections.mkstate conn);
  Connections.close_host_modal ();
  ()


let render () =
  Firebug.console##log (Js.string "rendering...");
  let demo =
    Js.Opt.get (d##getElementById(Js.string "demo"))
      (fun () -> assert false) in
  let states = Connections.all_states () in
  let xmls = List.map Pools.render_one states in
  let strs = List.map (fun xml -> Cow.Xml.to_string xml) xmls in
  let all = String.concat "" strs in
  Firebug.console##log (Js.string ("I'm supposed to be setting innerHTML to: " ^ all));
  demo##innerHTML <- Js.string all;
  Pools.connect_handlers ()
  
let onload _ =
  Connections.init ();

  let login_button =
    Js.Opt.get (d##getElementById(Js.string "login_button"))
      (fun () -> assert false) in

  login_button##onclick <- Dom_html.handler (fun _ -> action (); Js._true);

  let chart = C3.generate "#chart" C3.example in
  C3.flow chart ~flow_to:(`Delete 0) C3.flow_example;

  Pools.rerender := render;
  
  render ();
  Js._true

let _ =
  Dom_html.window##onload <- Dom_html.handler onload
