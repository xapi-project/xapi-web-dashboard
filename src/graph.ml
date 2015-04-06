open Lwt

let do_get ~uri =
  let method_ = "GET" in
  let (res, w) = Lwt.task () in
  let req = XmlHttpRequest.create () in

  Firebug.console##log(Js.string (Uri.to_string uri));
  req##_open (Js.string method_, Js.string (Uri.to_string uri), Js._true);
  req##onreadystatechange <- Js.wrap_callback
    (fun _ ->
       (match req##readyState with
                   | XmlHttpRequest.DONE ->
                           Lwt.wakeup w (Js.to_string req##responseText)
                   | _ -> ()));

	req##send (Js.some (Js.string ""));
  Lwt.on_cancel res (fun () -> req##abort ()) ;
  res

let watch_rrds { Connections.host; session } =
  let host = Uri.make ~scheme:"http" ~host () in
	let rec loop start =
    let uri = Xen_api_metrics.Updates.uri
	    ~host ~authentication:(`Session_id session)
		  ~start ~include_host:true
		  () in
		do_get ~uri
		>>= fun txt ->
		let update = Xen_api_metrics.Updates.parse txt in
		Firebug.console##log(Js.string "got some updates");
		Lwt_js.sleep 5.
		>>= fun () ->
		loop (Int64.to_int update.Rrd_updates.end_time) in
	(* XXX: query server's current clock *)
	loop 999999999
