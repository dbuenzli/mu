(* This code is in the public domain *)

open Mu
open Mu.Syntax

let dyn m l = M.phrase [`Dyn (`Loudness l)] m
let fff_to_ppp = [ `Fff; `Ff; `F; `Mf; `Mp ; `P; `Pp; `Ppp]

let c_major = M.chord M.[c 4 dhn; e 4 dhn; g 4 dhn] ^ M.qnr

let song = M.line (List.map (dyn c_major) fff_to_ppp)

let main () = Mu_player.main (Music.map Pnote.of_pitch song)
let () = if !Sys.interactive then () else main ()
