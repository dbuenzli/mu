(* This code is in the public domain *)

open Mu
open Mu.Syntax

let short_par m0 m1 = M.drop (Music.dur m1) m0 @|@ M.drop (Music.dur m0) m1

(* HSoM 6.8 *)

let rec trill : Pitch.rel -> Music.dur -> Pitch.t Music.t -> Pitch.t Music.t =
fun r d -> function
| `Prim (`Note (nd, _)) as n when Q.(d >= nd) -> n
| `Prim (`Note (nd, p)) ->
    M.note d p ^ trill (-r) d (M.note Q.(nd - d) (Pitch.transp r p))
| `Ctrl (`Tempo rat, m) -> M.tempo rat (trill r Q.(d * rat) m)
| `Ctrl (c, m) -> M.ctrl c (trill r d m)
| _ -> invalid_arg "Can only apply trill to single notes"

let trill_n r n m = trill r Q.(Music.dur m / int n) m
let trill_n' r n m = trill_n (-r) n (M.transp r m)

let roll dur m = trill 0 dur m
let rolln n m = trill_n 0 n m

let ssf_mel = (* Trills in the Stars and Stripes Forever HSoM fig. 6.2 *)
  let l1 = M.line M.[trill_n 2 5 (bf 6 en); ef 7 en; ef 6 en; ef 7 en] in
  let l2 = M.line M.[bf 6 sn; c 7 sn; bf 6 sn; g 6 sn; ef 6 en; bf 5 en] in
  let l3 = M.line M.[ef 6 sn; f 6 sn; g 6 sn; af 6 sn; bf 6 en; ef 7 en] in
  let l4 = M.line M.[trill 2 tsn (bf 6 qn); bf 6 sn; denr] in
  M.line [l1; l2; l3; l4]

(* HSoM 6.10 *)

let funk_groove =
  let p1 = M.(perc `Low_tom qn) in
  let p2 = M.(perc `Acoustic_snare en) in
  let l0 = M.(p1 ^ qnr ^ p2 ^ qnr ^ p2 ^ p1 ^ p1 ^ qnr ^ p2 ^ enr) in
  let l2 = roll M.en (M.perc `Closed_hi_hat Q.(int 2)) in
  let line = l0 @|@ l2 in
  M.tempo Q.(int 3) @@ M.times 4 line

(* HsoM 6.13 *)

let phase f m = m @|@ M.tempo f m
let phase1 s = phase Q.(3 #/ 2) (M.times 4 s)
let phase2 s = phase Q.(11 #/ 10) (M.times 4 s)
let phase3 s = phase Q.(101 #/ 100) (M.times 4 s)

(* HSoM 6.14 Crazy recursion *)

let rec rep f g n m = match n with
| 0 -> M.nil
| n -> m @|@ g (rep f g (n - 1) (f m))

let run = rep (M.transp 5) M.(offset tsn) 8 M.(c 4 tsn)
let cascade = rep (M.transp 4) M.(offset tsn) 8 run
let cascades = rep Fun.id M.(offset sn) 2 cascade
let final = cascades ^ M.retro cascades

let run' = rep M.(offset tsn) (M.transp 5) 8 M.(c 4 tsn)
let cascade' = rep M.(offset tsn) (M.transp 4) 8 run'
let cascades' = rep M.(offset sn) Fun.id 2 cascade'
let final' = cascades' ^ M.retro cascades'

let song = final

let main () = Mu_player.main (Music.map Pnote.of_pitch song)
let () = if !Sys.interactive then () else main ()
