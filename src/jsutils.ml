(* Simple jsoo utils *)

let data_attr_of_event ev data_name =
  let target : Dom_html.element Js.t = Dom.eventTarget ev in

  let rec loop (target: Dom_html.element Js.t) =
    let attr = target##attributes in
    let host = attr##getNamedItem (Js.string (Printf.sprintf "data-%s" data_name)) in
    match Js.Opt.to_option host with
    | None ->
      let ( >>= ) = Js.Opt.bind in
      let parent =
        target##parentNode
        >>= fun parent_node ->
        Dom.CoerceTo.element parent_node
        >>= fun parent ->
        Js.Opt.return (Dom_html.element parent) in

      begin match Js.Opt.to_option parent with
      | None ->
        Firebug.console##log (Js.string (Printf.sprintf "Failed to find item named data-%s" data_name));
        failwith "data_attr_of_event failed to find element"
      | Some parent ->
        loop parent
      end
    | Some host ->
      let v = host##value in
      Js.to_string v in
  loop target

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
