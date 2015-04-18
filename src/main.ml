open Jsutils

let d = Dom_html.document

open Lwt

let render () =
  Firebug.console##log (Js.string "rendering...");
  (* This is the main element: *)
  let demo = get_by_id "demo" in
  (* If there are no connections i.e. this is a first time use, trigger the
     add host modal immediately. *)
  let states = Connections.all_states () in
  if states = [] then begin
    Connections.open_host_modal ()
  end else begin

    let v = get_val "search_text_box" in
    let search = Search.query v in
    let xmls = List.map (function
      | `VM (p,r,vm_rec) ->
        if vm_rec.API.vM_is_a_template then None else Some (Vms.vm r vm_rec)
      | `Host (p,r,host_rec) ->
        Some (Hosts.host r host_rec)
      | `Pool (r,pool_rec) ->
        let st = List.find (fun st -> st.Connections.pool_ref = r) states in
        Some (Pools.render_one st)
      | `Message (p,r,message_rec) ->
        Some (Messages.message (r, message_rec))
    ) search in
    let xmls = List.fold_left (fun acc x -> match x with Some y -> y::acc | None -> acc) [] xmls in
    let disconnected = List.filter (fun st -> st.Connections.st <> Connections.Connected) states in
    let more_xml = List.map Pools.render_one disconnected in
    let strs = List.map (fun xml -> Cow.Xml.to_string xml) (more_xml @ xmls) in
    let all = String.concat "" strs in

    Firebug.console##log (Js.string "setting innerHTML");
    demo##innerHTML <- Js.string all;
    Pools.connect_handlers ();
    Vms.connect_handlers ();
    Hosts.connect_handlers ();
    Joyride.start ();
    (* Dropdowns require us to reinitialise foundation again *)
    Js.Unsafe.fun_call (Js.Unsafe.variable "reinitialise_foundation") [| |];
  end;
  Firebug.console##log (Js.string "... render complete")

let action () =
  let server = get_val "login_server" in
  let username = get_val "login_username" in
  let password = get_val "login_password" in
  let conn = Connections.remember_connection server username password in
  let (_: 'a Lwt.t) =
    Connections.connect (Connections.add conn) >>= fun (st, th) ->
    render ();
    th >>= fun () ->
    st.st <- Connected;
    render ();
    Lwt.return ()
  in
  Connections.close_host_modal ();
  ()



let onload _ =
  Connections.init ();

  let login_button = get_by_id "login_button" in
  login_button##onclick <- Dom_html.handler (fun _ -> action (); Js._true);

  let search_button = get_by_id "search_text_button" in
  search_button##onclick <- Dom_html.handler (fun _ -> render (); Js._true);

  let input = get_by_id "search_text_box" in
  let input_node = Js.Opt.get (Dom_html.CoerceTo.input input) (fun _ -> assert false) in
  input_node##onchange <- Dom_html.handler (fun _ -> render (); Js._true);

  let connect_icon_button (button,v) =
    let button = get_by_id button in
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
