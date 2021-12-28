(* This code is in the public domain. *)

open Mu
open Mu.Syntax

(* HSoM 4.4 *)

let rec prefixes = function
| [] -> [] | v :: vs -> [v] :: List.map (fun l -> v :: l) (prefixes vs)

let prefix mel =
  let cat_prefixes mel = List.concat (prefixes mel) in
  let m1 = M.line (cat_prefixes mel) in
  let m2 = M.transp 12 (M.line (cat_prefixes (List.rev mel))) in
  let m = M.instrument `Flute m1 @|@ M.instrument `Voice_oohs m2 in
  m ^ M.transp 5 m ^ m

let mel1 =
  M.[c 5 en; e 5 sn; g 5 en; b 5 sn; a 5 en; f 5 sn; d 5 en; b 4 sn; c 5 en]

let mel2 =
  M.[c 5 sn; e 5 sn; g 5 sn; b 5 sn; a 5 sn;f 5 sn; d 5 sn; b 4 sn; c 5 sn]

let song1 = prefix mel1
let song2 = prefix mel2

let main () = Mu_player.main (Music.map Pnote.of_pitch song2)
let () = if !Sys.interactive then () else main ()
