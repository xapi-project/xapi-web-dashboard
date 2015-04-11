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
