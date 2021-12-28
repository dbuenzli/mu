(* This code is in the public domain *)

open Mu
open Mu.Syntax

let art m a = M.phrase [`Art a] m
let arts = [ `Staccato Q.(1 #/ 4);
             `Legato Q.(int 4);
             `Slurred Q.(int 4); ]

let boogie = M.line M.[c 4 en; e 4 en; g 4 en; a 4 en]

let song = boogie ^ M.line (List.map (art boogie) arts)

let main () = Mu_player.main (Music.map Pnote.of_pitch song)
let () = if !Sys.interactive then () else main ()
