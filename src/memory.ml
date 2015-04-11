
open Int64
let ( ** ) = Int64.mul
let ( // ) = Int64.div

let kib = 1024L
let mib = 1024L ** kib
let gib = 1024L ** mib
let tib = 1024L ** gib

let to_string bytes =
  if bytes // tib > 0L
	then Printf.sprintf "%Ld TiB" (bytes // tib)
	else
	  if bytes // gib > 0L
		then Printf.sprintf "%Ld GiB" (bytes // gib)
		else Printf.sprintf "%Ld MiB" (bytes // mib)
