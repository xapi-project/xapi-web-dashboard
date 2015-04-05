type column_type =
  | Line
  | Area
  | Area_spline
  | Area_step

type column = {
  label: string;
  values: float list;
  ty: column_type;
}

type axis_ty =
  | Timeseries

type axis = {
  tics: string list; (* eg '2012-12-29' *)
  ty: axis_ty;
  format: string; (* eg '%m/%d' *)
}

type data = {
  x_axis: axis option;
  columns: column list;
}

let example = {
  x_axis = Some {
    tics = [ "2012-12-29"; "2012-12-30"; "2012-12-31"; "2013-01-01"; "2013-01-02"; "2013-01-03"];
    ty = Timeseries;
    format = "%m/%d";
  };
  columns = [
    { label = "data1";
      values = [ 30.; 200.; 100.; 400.; 150.; 250. ];
      ty = Area_step;
    }; {
      label = "data2";
      values = [ 50.; 20.; 10.; 40.; 15.; 25. ];
      ty = Line;
    }
  ]
}

let string_of_column_type = function
  | Line -> "line"
  | Area -> "area"
  | Area_spline -> "area-spline"
  | Area_step -> "area-step"

let string_of_axis_ty = function
  | Timeseries -> "timeseries"

let generate bindto data =
  let data_columns =
    Js.Unsafe.(
      List.map (fun column ->
        Js.array (Array.of_list (inject (Js.string column.label) :: (List.map inject column.values)))
      ) data.columns
    ) in

  let columns =
    Js.Unsafe.(
      inject (Js.array (Array.of_list (
        match data.x_axis with
        | None -> data_columns
        | Some x_axis ->
          let tics = Js.array (Array.of_list (inject (Js.string "x") :: (List.map (fun x -> inject (Js.string x)) x_axis.tics))) in
          tics :: data_columns
      )))
    ) in

  let axis =
    Js.Unsafe.(
      match data.x_axis with
      | None -> []
      | Some x -> [ "axis", obj [|
        "x", obj [|
          "type", inject (Js.string (string_of_axis_ty x.ty));
          "tick", obj [|
            "format", inject (Js.string x.format)
          |]
        |]
      |] ]
    ) in

    let data =
      Js.Unsafe.(
        (if data.x_axis = None then [] else [ "x", inject (Js.string "x") ]) @ [
          "columns", columns;
          "types", obj (Array.of_list (List.map (fun column ->
            column.label, inject (Js.string (string_of_column_type column.ty))
          ) data.columns));
        ]
      ) in

  let arg =
    Js.Unsafe.(obj
      (Array.of_list
        (axis @ [
          "bindto", inject (Js.string bindto);
          "data", obj (Array.of_list data)
        ])
      ))  in
  Firebug.console##log(arg);

  let c3 = Js.Unsafe.global##c3 in

  Js.Unsafe.meth_call c3 "generate" [| arg |]
