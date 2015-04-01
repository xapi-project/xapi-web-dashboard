
let testhtml n =
  <:html<
      <section class="hero">
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
  Connections.remember_host server username password

open Lwt

let onload _ =
  let _ = Connections.iter_hosts (fun f ->
      (Connections.connect f >>= fun i ->
      Vms.render i);
      ()) in

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
