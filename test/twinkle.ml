(* This code is in the public domain *)

open Mu
open Mu.Syntax

(* Twinkle Twinkle Little Star. Score in figure 4.1 of HSoM *)

let song =
  let qn_note pc = M.(note qn (pc, 4)) in
  let qn_line pcs = M.line (List.map qn_note pcs) in
  let m1 = qn_line [`C; `C; `G; `G; `A; `A] ^ M.(g 4 hn) in
  let m2 = qn_line [`F; `F; `E; `E; `D; `D] ^ M.(c 4 hn) in
  let m3 = qn_line [`G; `G; `F; `F; `E; `E] ^ M.(d 4 hn) in
  M.line [m1; m2; m3; m3; m1; m2]

let main () = Mu_player.main (Music.map Pnote.of_pitch song)
let () = if !Sys.interactive then () else main ()
