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

let graph_thread : unit Lwt.t option ref = ref None
let current_ds = ref None
let current_interval = ref (`Other 5)

let rec chart_handler ev =
  let host_ref = Jsutils.data_attr_of_event ev "host" in
  let states = Connections.all_states () in
  let (pool_ref,_) = Cache.M.find host_ref !Cache.host in
  let st = List.find (fun state -> state.pool_ref = pool_ref) states in


  let (_: 'a Lwt.t) =

    Client.Host.get_data_sources st.rpc st.session host_ref
    >>= function
    | [] ->
      Firebug.console##log(Js.string "Host has no datasources; can't draw a graph");
      return ()
    | (x :: xs) as all ->
      let ds_name = match Jsutils.get_attribute_of_target ev "data-ds" with
        | None -> !current_ds (* keep the current setting *)
        | Some ds -> Some ds in
      let ds = match ds_name with
      | None ->
       x (* arbitrary choice *)
      | Some name ->
        begin
          try
            List.find (fun ds -> ds.API.data_source_name_label = name) all
          with Not_found ->
            Firebug.console##log(Js.string (Printf.sprintf "Unknown data-source: %s" name));
            x
        end in
    let items = List.map
      (fun ds ->
        <:xml< <li data-host="$str:host_ref$" data-ds="$str:ds.API.data_source_name_label$" class="btn_metrics_change">$str:ds.API.data_source_name_description$</li> >>
      ) all in
    let all = String.concat " " (List.map Cow.Xml.to_string items) in
    let ul =
      Js.Opt.get (Dom_html.document##getElementById(Js.string "metrics-drop"))
        (fun () -> assert false) in
    ul##innerHTML <- Js.string all;
    (* Have we updated the interval? *)
    let interval = match Jsutils.get_attribute_of_target ev "data-interval" with
      | None ->
      Firebug.console##log(Js.string "XXX no change to interval");
      !current_interval
      | Some x -> `Other (int_of_string x) in

    let ul =
      Js.Opt.get (Dom_html.document##getElementById(Js.string "resolution-drop"))
        (fun () -> assert false) in
    let items = List.map
      (fun (seconds, descr) ->
        <:xml< <li data-host="$str:host_ref$" data-interval="$str:string_of_int seconds$" class="btn_resolution_change">$str:descr$</li> >>
      ) [
        5, "Sampled every 5 seconds for 10 minutes";
        60, "Sampled every minute for 2 hours";
        60 * 60, "Sampled every hour for 1 week";
        24 * 60 * 60, "Sampled every day for 1 year";
      ] in
      let all = String.concat " " (List.map Cow.Xml.to_string items) in
    ul##innerHTML <- Js.string all;

    Jsutils.connect_handler "btn_metrics_change" chart_handler;
    Jsutils.connect_handler "btn_resolution_change" chart_handler;

    begin match !graph_thread with
    | Some t -> Lwt.cancel t
    | None -> ()
    end;

    let chart = C3.generate "#chart" C3.example in
    let th = Graph.watch_rrds chart ds interval st in
    graph_thread := Some th;
    current_ds := Some ds.API.data_source_name_label;
    current_interval := interval;

    return () in


  Graph.open_chart_modal ();
  Js._true

let _modelname = "modelname"
let _cpu_count = "cpu_count"
let _socket_count = "socket_count"

let host host_ref host_rec =
  let memory =
    try
      let m = host_rec.API.host_metrics in
      let (_, host_metrics_rec) = Cache.M.find m !Cache.host_metrics in
      Memory.to_string host_metrics_rec.API.host_metrics_memory_total
    with Not_found ->
      "Unknown" in
  let current_ops = match host_rec.API.host_current_operations with
  | [] -> <:xml< >>
  | ops ->
    let ops = String.concat "," (List.map (fun (_,op) -> Rpc.to_string (API.rpc_of_host_allowed_operations op)) ops) in
    <:xml< <li><strong>Current ops: </strong>$str:ops$</li> >> in
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
  let address = host_rec.API.host_address in
  let cpu_model =
    if List.mem_assoc _modelname host_rec.API.host_cpu_info
    then List.assoc _modelname host_rec.API.host_cpu_info
    else "Unknown" in
  let cpu_details =
    if ((List.mem_assoc _cpu_count host_rec.API.host_cpu_info)
    && (List.mem_assoc _socket_count host_rec.API.host_cpu_info))
    then
      (List.assoc _cpu_count host_rec.API.host_cpu_info) ^ " cores arranged over " ^
      (List.assoc _socket_count host_rec.API.host_cpu_info) ^ " sockets"
    else
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
      $current_ops$
      <li><strong>Memory: </strong>$str:memory$</li>
      <li><strong>IP address: </strong>$str:address$</li>
          </ul>
        </div>
        <div class="medium-4 small-12 columns">
        <p>$str:cpu_model$</p>
        <p>$str:cpu_details$</p>
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
  List.iter (fun elt -> elt##onclick <- Dom_html.handler button_handler) (get_elts host_op_button);
  List.iter (fun elt -> elt##onclick <- Dom_html.handler chart_handler) (get_elts host_metrics_button);
