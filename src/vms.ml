open Connections
open Lwt

let vm_op_button = "btn_vm_op"

let button_handler ev =
  let vm_ref = Jsutils.data_attr_of_event ev "vm" in
  let op = Jsutils.data_attr_of_event ev "op" in
  let (pool_ref,vm_rec) = Cache.M.find vm_ref !(Cache.vm) in
  let states = Connections.all_states () in
  let st = List.find (fun st -> st.pool_ref = pool_ref) states in
  let (_ : 'a Lwt.t) =
    match op with
    | "start" ->
      Firebug.console##log (Js.string ("starting VM"));
      Client.VM.start st.rpc st.session vm_ref false false
    | "shutdown" ->
      Firebug.console##log (Js.string ("shutting down VM"));
      Client.VM.hard_shutdown st.rpc st.session vm_ref
    | "reboot" ->
      Firebug.console##log (Js.string ("rebooting VM"));
      Client.VM.hard_reboot st.rpc st.session vm_ref
    | _ ->
      failwith "Unknown op"
  in
  Js._true

let vm vm_ref vm_rec =
  let power_state =
    match vm_rec.API.vM_power_state with
    | `Halted -> "Halted"
    | `Paused -> "Paused"
    | `Running -> "Running"
    | `Suspended -> "Suspended"
  in
  let button_of_allowed_op op =
    match op with
    | `start ->
      Some <:xml< <li data-op="start" data-vm="$str:vm_ref$" class="button btn_vm_op"><i class="fi-play"> </i></li> >>
    | `hard_shutdown ->
      Some <:xml< <li data-op="shutdown" data-vm="$str:vm_ref$" class="button btn_vm_op"><i class="fi-power"> </i></li> >>
    | `hard_reboot ->
      Some <:xml< <li data-op="reboot" data-vm="$str:vm_ref$" class="button btn_vm_op"><i class="fi-refresh"> </i></li> >>
    | _ ->
      None
  in
  let ops = List.fold_left (fun acc x -> match button_of_allowed_op x with Some r -> r::acc | None -> acc) [] vm_rec.vM_allowed_operations in
  <:xml<
    <div class="panel">
      <div class="row">
      <div class="medium-8 small-12 columns">
        <h3><a href="#">$str:vm_rec.API.vM_name_label$</a></h3>
        <h5 class="left subheader">$str:vm_rec.API.vM_name_description$</h5>
      </div>
      <div class="medium-4 small-12 columns right">
        <strong>Power state: </strong><span class="">$str:power_state$</span>
        <ul class="button-group even-6">$list:ops$</ul>
      </div>
      </div>
    </div>
   >>

let d = Dom_html.document
  
  
let render (rpc,info) =
  Client.VM.get_all_records rpc info.session >>= fun vms ->
  let vms = List.filter (fun (_,vm_rec) -> not vm_rec.API.vM_is_a_template) vms in
  let vms = List.map (fun (vm_ref,vm_rec) -> Cow.Xml.to_string (vm vm_ref vm_rec)) vms in
  let demo =
    Js.Opt.get (d##getElementById(Js.string "demo"))
      (fun () -> assert false) in
  demo##innerHTML <- Js.string (String.concat "" vms);
  Lwt.return ()

let connect_handlers () =
  let elts = Dom.list_of_nodeList (d##getElementsByTagName(Js.string "li")) in
  let get_elts cls = List.filter (fun elt ->
      Js.to_bool (elt##classList##contains(Js.string cls))) elts
  in
  List.iter (fun elt -> elt##onclick <- Dom_html.handler button_handler) (get_elts vm_op_button);
