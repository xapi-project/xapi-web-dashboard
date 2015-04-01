open Connections
open Lwt

let render_disconnected id c =
  let top_id = Printf.sprintf "pool_%s" id in
  let connect_id = Printf.sprintf "btn_pool_connect_%s" id in
  <:xml< 
    <div class="panel" id="$str:top_id$">
      <div class="row">
        <div class="small-12 columns">
          <h5 class="left">Disconnected: $str:c.host$</h5>
	  <a href="#"  class="button  expand tiny" id="$str:connect_id$"><span><i class="fi-x"> </i>Connect</span> </a>
        </div>
      </div>
    </div>
  >>

let render_pool id name =
  let top_id = Printf.sprintf "pool_%s" id in
  let disconnect_id = Printf.sprintf "btn_pool_disconnect_%s" id in
  let forget_id = Printf.sprintf "btn_pool_forget_%s" id in
  <:xml< 
    <div class="panel" id="$str:top_id$">
      
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
	    <li><strong>Master: </strong>st28</li>
	    <li><strong>Master IP: </strong>10.80.3.4</li>
	    <li><strong>VMs: </strong>503</li>
	    <li><strong>hosts: </strong>16</li>
	  </ul>
	</div>
	
	<div class="medium-4 small-12 columns">
	  <p><a href="#"># VMs: 501</a></p>
	  <p><a href="#"># Hosts: 16</a></p>
	</div>

	<div class="medium-4 small-12 columns">
	  <a href="#"  class="button  expand tiny" id="$str:disconnect_id$"><span><i class="fi-x"> </i> Disconnect</span> </a>
	  <a href="#"  class="button  expand tiny" id="$str:forget_id$"><span><i class="fi-x"> </i> Forget</span></a> 
        </div>
	
      </div> 
    </div>
  >>,top_id,disconnect_id,forget_id


let d = Dom_html.document

let render f (rpc,info) =
  Client.Pool.get_all_records rpc info.session >>= fun pools ->
  let pool_ref,pool_rec = List.hd pools in
  (if String.length pool_rec.API.pool_name_label = 0
   then
     (Client.Pool.get_master rpc info.session pool_ref >>= fun master ->
      Client.Host.get_name_label rpc info.session master)
   else Lwt.return pool_rec.API.pool_name_label) >>= fun name ->
  let xml,top_id,disconnect_id,forget_id = render_pool pool_ref name in

  let remove_panel () =
    Js.Opt.iter (d##getElementById (Js.string top_id))
      (fun elt -> Dom.removeChild d elt)
  in

  remove_panel ();
  
  let demo =
    Js.Opt.get (d##getElementById(Js.string "demo"))
      (fun () -> assert false) in
  let str = Js.to_string demo##innerHTML in
  demo##innerHTML <- Js.string (str ^ (Cow.Xml.to_string xml));
  
  let handler _ =
    Connections.disconnect (rpc,info);
    Connections.forget_host f.host;
    remove_panel ();
    Js._true
  in
  
  let add_handler elt =
    Js.Opt.iter (d##getElementById (Js.string elt))
      (fun b -> b##onclick <- Dom_html.handler handler)
  in
  
  add_handler disconnect_id;
  add_handler forget_id;
      
  Lwt.return ()

