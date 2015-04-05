open Connections
open Lwt

let connect_btn = "btn_pool_connect"
let disconnect_btn = "btn_pool_disconnect"
let forget_btn = "btn_pool_forget"

let rerender = ref (fun () -> ())

let connect_handler ev =
  let d = Jsutils.data_attr_of_event ev "host" in
  let states = Connections.all_states () in
  let st = List.find (fun st -> st.c.host = d) states in
  let _ =
    Connections.connect st >>= fun (st, th) ->
    Firebug.console##log (Js.string ("moo " ^ st.c.host));
    (!rerender) ();
    th >>= fun () ->
    st.st <- Connected;
    (!rerender) ();
    Lwt.return ()
  in
  Js._true

let disconnect_handler ev =
  let d = Jsutils.data_attr_of_event ev "host" in
  let states = Connections.all_states () in
  let st = List.find (fun st -> st.c.host = d) states in
  let _ =
    Connections.disconnect st >>= fun st ->
    (!rerender) ();
    Lwt.return ()
  in
  Js._true

let forget_handler ev =
  let d = Jsutils.data_attr_of_event ev "host" in
  let states = Connections.all_states () in
  let st = List.find (fun st -> st.c.host = d) states in
  let _ =
    Connections.forget st >>= fun st ->
    (!rerender) ();
    Lwt.return ()
  in
  Js._true  

let render_disconnected st =
  let strstate = match st.st with
    | Disconnected -> <:xml< Disconnected >>
    | Connecting -> <:xml< Connecting >>
    | Connected -> <:xml< Uh, connected? >>
  in
  let host = st.c.host in
  <:xml< 
    <div class="panel">
      <div class="row">
	<div class="medium-8 small-12 columns">
	  <h3 class="left"><a href="#">$str:host$</a></h3>
	</div>
	<div class="medium-4 small-12 columns right">
	  <strong>State: </strong>$strstate$
	</div>
      </div>

      <div class="row">
        <div class="medium-4 small-12 columns">
	</div>
	
	<div class="medium-4 small-12 columns">
	</div>

	<div class="medium-4 small-12 columns">
	  <a href="#" class="button expand tiny btn_pool_connect" data-host="$str:host$"><span><i class="fi-x"> </i>Connect</span> </a>
	  <a href="#" data-host="$str:host$" class="button expand tiny btn_pool_forget"><span><i class="fi-x"> </i>Forget</span></a> 
        </div>
      </div>
    </div>
  >>

let render_pool st id name master_name master_address =
  let host = st.c.host in
  <:xml< 
    <div class="panel" id="$str:id$">
      
      <div class="row">
	<div class="medium-8 small-12 columns">
	  <h3 class="left"><a href="#">$str:name$</a></h3>
	</div>
	<div class="medium-4 small-12 columns right">
	  <strong>State: </strong><span class="">Connected</span>
	</div>
      </div>
      
      <div class="row">
        <div class="medium-4 small-12 columns">
          <ul>
	    <li><strong>Master: </strong>$str:master_name$</li>
	    <li><strong>Master IP: </strong>$str:master_address$</li>
	    <li><strong>VMs: </strong>503</li>
	    <li><strong>hosts: </strong>16</li>
	  </ul>
	</div>
	
	<div class="medium-4 small-12 columns">
	  <p><a href="#"># VMs: 501</a></p>
	  <p><a href="#"># Hosts: 16</a></p>
	</div>

	<div class="medium-4 small-12 columns">
	  <a href="#" data-host="$str:host$" class="button expand tiny btn_pool_disconnect"><span><i class="fi-x"> </i>Disconnect</span> </a>
	  <a href="#" data-host="$str:host$" class="button expand tiny btn_pool_forget"><span><i class="fi-x"> </i>Forget</span></a> 
        </div>
	
      </div> 
    </div>
  >>

let d = Dom_html.document

let render_one st =
  match st.st with
  | Connected ->
    Firebug.console##log (Js.string (st.c.host ^ " is connected"));    
    let pool_ref = st.pool_ref in
    let pool_rec = Cache.M.find pool_ref !Cache.pool in
    let master = pool_rec.API.pool_master in
    let master_rec = Cache.M.find master !Cache.host in
    let pool_name =
      if String.length pool_rec.API.pool_name_label = 0
      then master_rec.API.host_name_label
      else pool_rec.API.pool_name_label in
    let address = master_rec.API.host_address in
    begin
      try
	let result = render_pool st pool_ref pool_name master_rec.API.host_name_label address in
        Firebug.console##log (Js.string ("called render_pool"));
        result
      with e ->
	Firebug.console##log (Js.string (Printexc.to_string e));
	raise e
    end
  | Connecting
  | Disconnected ->
    Firebug.console##log (Js.string (st.c.host ^ " is not connected yet"));    
    render_disconnected st


let connect_handlers () =
  let elts = Dom.list_of_nodeList (d##getElementsByTagName(Js.string "a")) in
  let get_elts cls = List.filter (fun elt ->
      Js.to_bool (elt##classList##contains(Js.string cls))) elts
  in
  List.iter (fun elt -> elt##onclick <- Dom_html.handler connect_handler) (get_elts connect_btn);
  List.iter (fun elt -> elt##onclick <- Dom_html.handler disconnect_handler) (get_elts disconnect_btn);
  List.iter (fun elt -> elt##onclick <- Dom_html.handler forget_handler) (get_elts forget_btn)
    
    
  
  
