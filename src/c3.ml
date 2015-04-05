
type column = {
  label: string;
  values: float list;
}

type data = { columns: column list }

let example = {
  columns = [
    { label = "data1"; values = [ 30.; 200.; 100.; 400.; 150.; 250. ] };
    { label = "data2"; values = [ 50.; 20.; 10.; 40.; 15.; 25. ] }
  ]
}

(* The javascript example was:

var chart = c3.generate({
  bindto: '#chart',
  data: {
    columns: [
      ['data1', 30, 200, 100, 400, 150, 250],
      ['data2', 50, 20, 10, 40, 15, 25]
    ]
  }
});

*)

let generate bindto data =
  let arg =
    Js.Unsafe.(obj
      [|"bindto", inject (Js.string bindto);
        "data", obj [|
          "columns", inject (Js.array (Array.of_list (List.map (fun column ->
            Js.array (Array.of_list (inject (Js.string column.label) :: (List.map inject column.values)))
          ) data.columns)))
        |]
      |])  in
  let c3 = Js.Unsafe.global##c3 in

  Js.Unsafe.meth_call c3 "generate" [| arg |]
