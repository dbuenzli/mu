(* This code is in the public domain *)

open Mu
open Mu.Syntax

let song = M.(c 4 wn)
let main () = Mu_player.main (Music.map Pnote.of_pitch song)
let () = if !Sys.interactive then () else main ()
