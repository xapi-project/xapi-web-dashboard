open Connections
open Lwt
  
let vm vm_rec =
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
      Some <:xml< <li class="button"><i class="fi-play"> </i></li> >>
    | `hard_shutdown ->
      Some <:xml< <li class="button"><i class="fi-power"> </i></li> >>
    | `hard_reboot ->
      Some <:xml< <li class="button"><i class="fi-refresh"> </i></li> >>
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
  let vms = List.map (fun (vm_ref,vm_rec) -> Cow.Xml.to_string (vm vm_rec)) vms in
  let demo =
    Js.Opt.get (d##getElementById(Js.string "demo"))
      (fun () -> assert false) in
  demo##innerHTML <- Js.string (String.concat "" vms);
  Lwt.return ()
