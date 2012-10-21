(*
  utils.ml
  Copyright (c) 2012- Masaki WATANABE
  license: see LICENSE
*)
type endian = LE | BE

let (@@) f g = f g
let (+>) f g = g f
let ($) f g x = f (g x)
let spf = Printf.sprintf

let be_of_le ~nbyte value : char list =
  let rec iter rest i =
    if i < nbyte then
      let b1 = char_of_int (rest land 0xFF) in
      let rest' = rest lsr 8 in
      b1 :: (iter rest' (i+1))
    else [] in
  List.rev @@ iter value 0
;;  
