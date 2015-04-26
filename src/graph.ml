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

let open_chart_modal () =
  Js.Unsafe.fun_call (Js.Unsafe.variable "open_chart_modal") [| |]

let close_chart_modal () =
  Js.Unsafe.fun_call (Js.Unsafe.variable "close_chart_modal") [| |]

let endswith suffix x =
  let suffix' = String.length suffix in
	let x' = String.length x in
	suffix' <= x' && (String.sub x (x' - suffix') suffix' = suffix)

let render_update chart data_source window update =
  let open Rrd_updates in
	let _, chosen = Array.fold_left
	  (fun (idx, chosen) elt ->
			if chosen >=0 then (idx + 1, chosen)
			else
        match Xen_api_metrics.Legend.of_string elt with
        | `Ok (name, _, _, _) ->
          if data_source.API.data_source_name_label = name
          then (idx + 1, idx)
          else (idx + 1, chosen)
        | _ ->
          (idx + 1, chosen)
    ) (0, -1) update.legend in
	if chosen = -1
	then Firebug.console##log(Js.string "Failed to find an interesting RRD update")
	else begin
	  (* Firebug.console##log(Js.string(Printf.sprintf "legends = %d; nrows = %d; nrow_datas = [ %s ]" (Array.length update.legend) (Array.length update.data) (String.concat "; " (List.map (fun x -> string_of_int (Array.length x.row_data)) (Array.to_list update.data)))));
    *)
		let points = List.map (fun x -> x.time, x.row_data.(chosen)) (Array.to_list update.data) in
    (* Filter out Nans *)
		let points = List.filter (fun (_, x) -> classify_float x <> FP_nan) points in
		Firebug.console##log(Js.string (Printf.sprintf "points = [| %s |]" (String.concat "; " (List.map (fun (t, p) -> Printf.sprintf "%Ld %f" t p) points))));
    (* XXX: I have no idea what these values mean! *)
		(*let point = sin(time /. 60.) *. point *. 0.2 +. point *. 0.8 in*)
		let data = (
			List.map (fun x -> Int64.to_float (fst x)) points,
			[
		    {
		      C3.label = data_source.API.data_source_name_description;
		      values = List.map snd points;
		      ty = Area_step;
		    }
			]
		) in
		C3.flow chart ~flow_to:(`ToX (float_of_int (Int64.to_int update.end_time - window))) data;

	end

let watch_rrds chart data_source interval { Connections.session; rpc; pool_ref; c = { Connections.host } } =
  let host = Uri.make ~scheme:"http" ~host () in
  let uri start = Xen_api_metrics.Updates.uri
      ~host ~authentication:(`Session_id session)
      ~start ~interval ~include_host:true
      () in
  (* Aim to keep this length of time on screen *)
  let length = Xen_api_metrics.seconds_of_interval interval in
  let window = Xen_api_metrics.archive_length_of_interval interval in
  let rec loop start =
    do_get ~uri:(uri start)
    >>= fun txt ->
    let update = Xen_api_metrics.Updates.parse txt in
    Firebug.console##log(Js.string (Printf.sprintf "updates start = %Ld end = %Ld range = %Ld"
      update.Rrd_updates.start_time update.Rrd_updates.end_time
      (Int64.sub update.Rrd_updates.end_time update.Rrd_updates.start_time)));
    render_update chart data_source window update;
    Lwt_js.sleep (float_of_int (Xen_api_metrics.seconds_of_interval interval))
    >>= fun () ->
    loop (Int64.to_int update.Rrd_updates.end_time) in
  do_get ~uri:(uri max_int) >>= fun txt ->
  let update = Xen_api_metrics.Updates.parse txt in
  (* Initially we ask for an update in the future, and this reveals
     the time of the last update. *)
  if update.Rrd_updates.end_time < update.Rrd_updates.start_time
  then loop (Int64.to_int update.Rrd_updates.end_time - window + length)
  else loop ((Int64.to_int update.Rrd_updates.end_time))
