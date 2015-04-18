
open Sexplib.Std

type t = bool with sexp

let st = LocalStorage.init_js ()

let of_string x = t_of_sexp (Sexplib.Sexp.of_string x)
let to_string x = Sexplib.Sexp.to_string (sexp_of_t x)

let key = Js.string "joyride"

let start () =
  let already_seen : bool = Js.Opt.case (st#get key)
    (fun () -> false)
    (fun s -> of_string (Js.to_string s))  in

  if not already_seen then begin
  let v = Js.Unsafe.global##document in
    Js.Unsafe.fun_call (Js.Unsafe.variable "start_joyride") [| |];
    st#set key (Js.string (to_string true));
    ()
  end
