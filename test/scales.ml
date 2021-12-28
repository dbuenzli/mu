(* This code is in the public domain *)

open Mu
open Mu.Syntax

type scale = Pitch.rel list

let major = [2; 2; 1; 2; 2; 2]
let hexatonic_blues = [3; 2; 1; 1; 3]

let make_scale : Pitch.t -> scale -> Pitch.t list =
fun p scale ->
  let next acc rel = Pitch.transp rel (List.hd acc) :: acc in
  List.rev (List.fold_left next [p] scale)

let play_scale p scale =
  let up (p, o) = (p, o + 1) in
  M.tempo Q.(int 2) @@
  M.line (List.map M.(note qn) (make_scale p scale)) ^ M.(note qn (up p))

let base = (`C, 4)

let major = play_scale base major
let hexatonic_blues = play_scale base hexatonic_blues
let hexatonic_blues = hexatonic_blues ^ M.retro hexatonic_blues

let main () = Mu_player.main (Music.map Pnote.of_pitch hexatonic_blues)
let () = if !Sys.interactive then () else main ()
