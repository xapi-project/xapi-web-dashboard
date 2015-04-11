open Connections
open Lwt

let host_op_button = "btn_host_op"
let host_metrics_button = "btn_host_metrics"

let button_handler ev =
  let host_ref = Jsutils.data_attr_of_event ev "host" in
  let op = Jsutils.data_attr_of_event ev "op" in
  let (pool_ref,host_rec) = Cache.M.find host_ref !(Cache.host) in
  let states = Connections.all_states () in
  let st = List.find (fun st -> st.pool_ref = pool_ref) states in
  let (_ : 'a Lwt.t) =
    match op with
    | "evacuate" ->
      Firebug.console##log (Js.string ("evacuate host"));
      Client.Host.evacuate st.rpc st.session host_ref
    | "shutdown" ->
      Firebug.console##log (Js.string ("shutting down host"));
      Client.Host.shutdown st.rpc st.session host_ref
    | "reboot" ->
      Firebug.console##log (Js.string ("rebooting host"));
      Client.Host.reboot st.rpc st.session host_ref
    | _ ->
      failwith "Unknown op"
  in
  Js._true

let chart_handler ev =
  let host_ref = Jsutils.data_attr_of_event ev "host" in
  let states = Connections.all_states () in
  let (pool_ref,_) = Cache.M.find host_ref !Cache.host in
  let st = List.find (fun state -> state.pool_ref = pool_ref) states in
  Graph.open_chart_modal ();
  let chart = C3.generate "#chart" C3.example in
  let (_ : 'a Lwt.t) = Graph.watch_rrds chart st in
  Js._true


let host host_ref host_rec =
  let memory =
    try
      let m = host_rec.API.host_metrics in
      let (_, host_metrics_rec) = Cache.M.find m !Cache.host_metrics in
      Memory.to_string host_metrics_rec.API.host_metrics_memory_total
    with Not_found ->
      "Unknown" in
  let current_ops = String.concat "," (List.map (fun (_,op) -> Rpc.to_string (API.rpc_of_host_allowed_operations op)) host_rec.API.host_current_operations) in
  let button_of_allowed_op op =
    match op with
    | `evacuate ->
      Some <:xml< <li data-op="evacuate" data-host="$str:host_ref$" class="button btn_host_op"><i class="fi-play"> </i></li> >>
    | `shutdown ->
      Some <:xml< <li data-op="shutdown" data-host="$str:host_ref$" class="button btn_host_op"><i class="fi-power"> </i></li> >>
    | `reboot ->
      Some <:xml< <li data-op="reboot" data-host="$str:host_ref$" class="button btn_host_op"><i class="fi-refresh"> </i></li> >>
    | _ ->
      None
  in
  let power_state =
    try
      let m = host_rec.API.host_metrics in
      let (_, host_metrics_rec) = Cache.M.find m !Cache.host_metrics in
      match host_metrics_rec.API.host_metrics_live, host_rec.API.host_enabled with
      | true, true -> "Online"
      | true, false -> "Online but disabled"
      | false, _ -> "Not responding to heartbeats"
    with _ ->
      "Unknown" in
  let metrics_btn = <:xml< <li data-host="$str:host_ref$" class="button btn_host_metrics"><i class="fa fa-bar-chart"> </i></li> >> in
  let ops = List.fold_left (fun acc x -> match button_of_allowed_op x with Some r -> r::acc | None -> acc) [metrics_btn] host_rec.API.host_allowed_operations in
  <:xml<
    <div class="panel">
      <div class="row">
        <div class="medium-8 small-12 columns">
          <h4><a href="#">$str:host_rec.API.host_name_label$</a></h4>
          <p class="left subheader">$str:host_rec.API.host_name_description$</p>
        </div>
        <div class="medium-4 small-12 columns right">
          <strong>Power state: </strong><span class="">$str:power_state$</span>
        </div>
      </div>
      <div class="row">
        <div class="medium-4 small-12 columns">
          <ul>
	    <li><strong>Current ops: </strong>$str:current_ops$</li>
	    <li><strong>Memory: </strong>$str:memory$</li>
          </ul>
        </div>
        <div class="medium-4 small-12 columns">
          <p>more info?</p>
	        <p>more info?</p>
        </div>
        <div class="medium-4 small-12 columns">
          <ul class="button-group even-6">$list:ops$</ul>

        </div>
      </div>
    </div>
   >>

let d = Dom_html.document


let render (rpc,info) =
  Client.Host.get_all_records rpc info.session >>= fun hosts ->
  let hosts = List.map (fun (host_ref,host_rec) -> Cow.Xml.to_string (host host_ref host_rec)) hosts in
  let demo =
    Js.Opt.get (d##getElementById(Js.string "demo"))
      (fun () -> assert false) in
  demo##innerHTML <- Js.string (String.concat "" hosts);
  Lwt.return ()

let connect_handlers () =
  let elts = Dom.list_of_nodeList (d##getElementsByTagName(Js.string "li")) in
  let get_elts cls = List.filter (fun elt ->
      Js.to_bool (elt##classList##contains(Js.string cls))) elts
  in
  List.iter (fun elt -> elt##onclick <- Dom_html.handler button_handler) (get_elts host_op_button);
  List.iter (fun elt -> elt##onclick <- Dom_html.handler chart_handler) (get_elts host_metrics_button);
