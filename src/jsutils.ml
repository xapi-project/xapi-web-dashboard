(* Simple jsoo utils *)

let data_attr_of_event ev data_name =
  let target = Dom.eventTarget ev in
  let attr = target##attributes in
  let host = attr##getNamedItem (Js.string (Printf.sprintf "data-%s" data_name)) in
  let host = Js.Opt.get host (fun () -> failwith "foo") in
  let v = host##value in
  Js.to_string v
