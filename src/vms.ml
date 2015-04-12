open Connections
open Lwt

let vm_op_button = "btn_vm_op"
let vm_metrics_button = "btn_vm_metrics"

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

let chart_handler ev =
  let vm_ref = Jsutils.data_attr_of_event ev "vm" in
  let states = Connections.all_states () in
  let (pool_ref,_) = Cache.M.find vm_ref !Cache.vm in
  let st = List.find (fun state -> state.pool_ref = pool_ref) states in
  Graph.open_chart_modal ();
  let chart = C3.generate "#chart" C3.example in
  let (_ : 'a Lwt.t) = Graph.watch_rrds chart st in
  Js._true


let vm vm_ref vm_rec =
  let memory = Memory.to_string vm_rec.API.vM_memory_static_max in
  let vcpus = Printf.sprintf "%Ld" vm_rec.API.vM_VCPUs_max in
  let vbds = string_of_int (List.length vm_rec.API.vM_VBDs) in
  let vifs = string_of_int (List.length vm_rec.API.vM_VIFs) in
  let current_ops = match vm_rec.API.vM_current_operations with
  | [] -> <:xml< >>
  | ops ->
    let ops = String.concat "," (List.map (fun (_,op) -> Rpc.to_string (API.rpc_of_vm_operations op)) ops) in
    <:xml< <li><strong>Current ops: </strong>$str:ops$</li> >> in
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
  let metrics_btn = <:xml< <li data-vm="$str:vm_ref$" class="button btn_vm_metrics"><i class="fa fa-bar-chart"> </i></li> >> in
  let ops = List.fold_left (fun acc x -> match button_of_allowed_op x with Some r -> r::acc | None -> acc) [metrics_btn] vm_rec.API.vM_allowed_operations in
  <:xml<
    <div class="panel">
      <div class="row">
        <div class="medium-8 small-12 columns">
          <h4><a href="#">$str:vm_rec.API.vM_name_label$</a></h4>
          <p class="left subheader">$str:vm_rec.API.vM_name_description$</p>
        </div>
        <div class="medium-4 small-12 columns right">
          <strong>Power state: </strong><span class="">$str:power_state$</span>
        </div>
      </div>
      <div class="row">
        <div class="medium-4 small-12 columns">
          <ul>
          $current_ops$
      <li><strong>Memory: </strong>$str:memory$</li>
	    <li><strong>VCPUs: </strong>$str:vcpus$</li>
          </ul>
        </div>
        <div class="medium-4 small-12 columns">
          <p>VBDs: $str:vbds$</p>
	  <p>VIFs: $str:vifs$</p>
        </div>
        <div class="medium-4 small-12 columns">
          <ul class="button-group even-6">$list:ops$</ul>

        </div>
      </div>
    </div>
   >>

let d = Dom_html.document

let connect_handlers () =
  let elts = Dom.list_of_nodeList (d##getElementsByTagName(Js.string "li")) in
  let get_elts cls = List.filter (fun elt ->
      Js.to_bool (elt##classList##contains(Js.string cls))) elts
  in
  List.iter (fun elt -> elt##onclick <- Dom_html.handler button_handler) (get_elts vm_op_button);
  List.iter (fun elt -> elt##onclick <- Dom_html.handler chart_handler) (get_elts vm_metrics_button);
