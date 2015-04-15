(* Simple jsoo utils *)

let data_attr_of_event ev data_name =
  let target = Dom.eventTarget ev in
  let attr = target##attributes in
  let host = attr##getNamedItem (Js.string (Printf.sprintf "data-%s" data_name)) in
  let host = Js.Opt.get host (fun () ->
     Firebug.console##log (Js.string (Printf.sprintf "Failed to find item named data-%s" data_name));
     failwith "data_attr_of_event failed to find element") in
  let v = host##value in
  Js.to_string v

let get_attribute_of_target ev data =
  let target = Dom.eventTarget ev in
  let attr = target##attributes in
  let host = attr##getNamedItem (Js.string data) in
  match Js.Opt.to_option host with
  | None -> None
  | Some host ->
    let v = host##value in
    Some (Js.to_string v)

let connect_handler name handler =
  Dom_html.document##getElementsByTagName (Js.string "li")
  |> Dom.list_of_nodeList
  |> List.filter (fun elt -> Js.to_bool (elt##classList##contains(Js.string name)))
  |> List.iter (fun elt -> elt##onclick <- Dom_html.handler handler)
