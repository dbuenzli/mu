(* This code is in the public domain *)

open Mu
open Mu.Syntax

let pair p step =
  M.note M.qn (Pitch.of_abs (p + step), 7 + step * 10 ) @|@
  M.note M.qn (Pitch.of_abs (p + step + 12), 127 - step * 10)

let up p =
  let rec loop step p acc =
    if step >= 12 then acc else
    let m = pair p step ^ M.rest M.qn in
    loop (step + 1) p (acc ^ m)
  in
  loop 0 (Pitch.to_abs p) M.nil

let shepard = up (`C, 1) @|@ up (`C, 3)

let song = M.tempo Q.(3 #/ 1) (M.times 8 shepard)

let main () = Mu_player.main (Music.map Pnote.of_pitch_volume song)
let () = if !Sys.interactive then () else main ()
