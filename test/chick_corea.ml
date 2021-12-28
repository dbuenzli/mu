(* This code is in the public domain *)

open Mu
open Mu.Syntax

(* Children's Songs No. 6. Chick Corea. Score in figure 4.2 of HSoM *)

let app_dur d ns = M.line (List.map (fun n -> n d) ns)
let grace_note n m =
  let d, p = match m with
  | `Prim (`Note (d, p)) -> d, p | _ -> invalid_arg "not a note value"
  in
  M.note Q.(1 #/ 8 * d) (Pitch.transp n p) ^
  M.note Q.(7 #/ 8 * d) p

let bass_line =
  let b1 = app_dur M.dqn M.[b 3; fs 4; g 4; fs 4] in
  let b2 = app_dur M.dqn M.[b 3; es 4; fs 4; es 4] in
  let b3 = app_dur M.dqn M.[as' 3; fs 4; g 4; fs 4] in
  M.times 3 b1 ^ M.times 2 b2 ^ M.times 4 b3 ^ M.times 5 b1

let main_voice =
  let v1 =
    let v1a = app_dur M.en M.[a 5; e 5; d 5; fs 5; cs 5; b 4; e 5; b 4] in
    let v1b = app_dur M.en M.[cs 5; b 4] in
    v1a ^ grace_note (-1) M.(d 5 qn) ^ v1b
  in
  let v2 =
    let v2a =
      M.line M.[cs 5 Q.(dhn + dhn); d 5 dhn; f 5 hn; gs 5 qn;
                fs 5 Q.(hn + en); g 5 en ]
    in
    let v2b =
      app_dur M.en M.[fs 5; e 5; cs 5; as' 4] ^ M.(a 4 dqn) ^
      app_dur M.en M.[as' 4; cs 5; fs 5; e 5; fs 5]
    in
    let v2c =
      M.line M.[g 5 en; as' 5 en; cs 6 Q.(hn + en); d 6 en; cs 6 en] ^
      M.(e 5 en) ^ M.enr ^
      M.line M.[as' 5 en; a 5 en; g 5 en; d 5 qn; c 5 en; cs 5 en]
    in
    let v2d =
      app_dur M.en M.[fs 5; cs 5; e 5; cs 5; a 4; as' 4; d 5; e 5; fs 5]
    in
    let v2e =
      M.line M.[grace_note 2 (e 5 qn); d 5 en; grace_note 2 (d 5 qn); cs 5 en;
                grace_note 1 (cs 5 qn); b 4 Q.(en + hn); cs 5 en; b 4 en ]
    in
    let v2f =
      M.line M.[fs 5 en; a 5 en; b 5 Q.(hn + qn); a 5 en; fs 5 en; e 5 qn;
                d 5 en; fs 5 en; e 5 hn; d 5 hn; fs 5 qn; ]
    in
    let v2g =
      M.tempo Q.(3 #/ 2) (M.line M.[cs 5 en; d 5 en; cs 5 en]) ^
      M.(b 4 Q.(int 3 * dhn + hn))
    in
    M.line [v2a; v2b; v2c; v2d; v2e; v2f; v2g]
  in
  M.times 3 v1 ^ v2

let child_song_6 =
  let t = Q.(M.(dhn / qn) * (69 #/ 120)) in
  M.instrument `Rhodes_piano (M.tempo t (bass_line @|@ main_voice))

let main () = Mu_player.main (Music.map Pnote.of_pitch child_song_6)
let () = if !Sys.interactive then () else main ()
