
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

let set_val id v =
  let input = get_by_id id in
  let input_node = Js.Opt.get (Dom_html.CoerceTo.input input) (fun _ -> assert false) in
  input_node##value <- Js.string v

open Lwt

let action () =
  let server = get_val "login_server" in
  let username = get_val "login_username" in
  let password = get_val "login_password" in
  let conn = Connections.remember_connection server username password in
  let (_: 'a Lwt.t) =
    Connections.connect (Connections.mkstate conn);
    >>= fun (st, _) ->
    (* Start displaying a graph, for fun *)
    let chart = C3.generate "#chart" C3.example in
    Graph.watch_rrds chart st in

  Connections.close_host_modal ();
  ()


let render () =
  Firebug.console##log (Js.string "rendering...");
  let demo =
    Js.Opt.get (d##getElementById(Js.string "demo"))
      (fun () -> assert false) in
  let states = Connections.all_states () in
  let v = get_val "search_text_box" in
  let search = Search.query v in
  let xmls = List.map (function
    | `VM (p,r,vm_rec) ->
      if vm_rec.API.vM_is_a_template then None else Some (Vms.vm r vm_rec)
    | `Host (p,r,host_rec) ->
      Some <:xml< <span>host</span> >>
    | `Pool (r,pool_rec) ->
      let st = List.find (fun st -> st.Connections.pool_ref = r) states in
      Some (Pools.render_one st)) search in
  let xmls = List.fold_left (fun acc x -> match x with Some y -> y::acc | None -> acc) [] xmls in
  let disconnected = List.filter (fun st -> st.Connections.st <> Connections.Connected) states in
  let more_xml = List.map Pools.render_one disconnected in
  let strs = List.map (fun xml -> Cow.Xml.to_string xml) (more_xml @ xmls) in
  let all = String.concat "" strs in
  Firebug.console##log (Js.string ("I'm supposed to be setting innerHTML to: " ^ all));
  demo##innerHTML <- Js.string all;
  Pools.connect_handlers ();
  Vms.connect_handlers ()

let onload _ =
  Connections.init ();

  let get_btn btn =
    Js.Opt.get (d##getElementById(Js.string btn))
      (fun () -> assert false) in

  let login_button = get_btn "login_button" in
  login_button##onclick <- Dom_html.handler (fun _ -> action (); Js._true);

  let search_button = get_btn "search_text_button" in
  search_button##onclick <- Dom_html.handler (fun _ -> render (); Js._true);

  let connect_icon_button (button,v) =
    let button = get_btn button in
    button##onclick <- Dom_html.handler (fun _ ->
	set_val "search_text_box" v;
	render (); Js._true)
  in
  List.iter connect_icon_button [
    ("icon_bar_pools","class:pool");
    ("icon_bar_hosts","class:host");
    ("icon_bar_vms","class:vm");
    ("icon_bar_alerts","class:message")];

  Cache.notify := render;

  Pools.rerender := render;

  render ();
  Js._true

let _ =
  Dom_html.window##onload <- Dom_html.handler onload
