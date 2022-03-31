(*---------------------------------------------------------------------------
   Copyright (c) 2021 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let pf = Format.fprintf
let strf = Printf.sprintf

(* Rational numbers *)

module Q = struct
  type t = { num : int; den : int }
  (* [num] and [den] have no common factors and [den] is always non-negative. *)

  let rec gcd a = function 0 -> a | b -> gcd b (a mod b)

  let q num den = (* assert den > 0 *)
    if num = 0 || den = 1 then { num; den = 1 } else
    match gcd num den with
    | 1 -> { num; den }
    | gcd ->  { num = num / gcd; den = den / gcd }

  let v num den = match den with
  | 0 ->
      let num = match num with 0 -> 0 | num when num < 0 -> -1 | _ -> 1 in
      { num; den}
  | den when den < 0 -> q (-num) (-den)
  | den -> q num den

  let int i = v i 1
  let ( #/ ) num den = v num den
  let num r = r.num
  let den r = r.den

  (* Constants *)

  let zero = 0 #/ 1
  let one = 1 #/ 1
  let minus_one = (-1) #/ 1
  let infinity = 1 #/ 0
  let neg_infinity = (-1) #/ 0
  let nan = 0 #/ 0

  (* Predicates and comparisons *)

  type kind = Nan | Neg_infinity | Infinity | Zero | Non_zero
  let kind r = match r.den with
  | 0 -> (match r.num with 1 -> Infinity | -1 -> Neg_infinity | _ -> Nan)
  | _ -> (match r.num with 0 -> Zero | _ -> Non_zero)

  let is_finite r = r.den <> 0
  let is_infinite r = r.den = 0 && r.num <> 0
  let is_nan r = r.den = 0 && r.num = 0
  let sign r = match r.num with 0 -> 0 | n when n < 0 -> -1 | _ -> 1

  let compare r0 r1 = match kind r0, kind r1 with
  | Nan, Nan | Infinity, Infinity | Neg_infinity, Neg_infinity -> 0
  | Nan, _ -> -1
  | _, Nan -> 1
  | Neg_infinity, _ | _, Infinity -> -1
  | Infinity, _ | _, Neg_infinity -> 1
  | _ when r0.den = r1.den -> Int.compare r0.num r1.num
  | _ -> Int.compare (r0.num * r1.den) (r1.num * r0.den)

  let equal r0 r1 = compare r0 r1 = 0
  let min r0 r1 = if compare r0 r1 <= 0 then r0 else r1
  let max r0 r1 = if compare r0 r1 >= 0 then r0 else r1

  (* Arithmetic *)

  let neg r = { r with num = -r.num }
  let abs r = { r with num = abs r.num }
  let add r0 r1 = match r0.den = 0 || r1.den = 0 with
  | false ->
      if r0.den = r1.den
      then q (r0.num + r1.num) r1.den
      else q (r0.num * r1.den + r1.num * r0.den) (r0.den * r1.den)
  | true ->
      match kind r0, kind r1 with
      | Zero, _ -> r1
      | _ , Zero -> r0
      | Nan, _ | _, Nan -> nan
      | Infinity, Neg_infinity | Neg_infinity, Infinity -> nan
      | Infinity, _ | _, Infinity -> infinity
      | Neg_infinity, _ | _, Neg_infinity -> neg_infinity
      | Non_zero, Non_zero -> assert false

  let sub r0 r1 = match r0.den = 0 || r1.den = 0 with
  | false ->
      if r0.den = r1.den
      then q (r0.num - r1.num) r1.den
      else q (r0.num * r1.den - r1.num * r0.den) (r0.den * r1.den)
  | true ->
      match kind r0, kind r1 with
      | Zero, _ -> neg r1
      | _ , Zero -> r0
      | Nan, _ | _, Nan -> nan
      | Infinity, Infinity | Neg_infinity, Neg_infinity -> nan
      | Infinity, _ | _, Neg_infinity -> infinity
      | Neg_infinity, _ | _, Infinity -> neg_infinity
      | Non_zero, Non_zero -> assert false

  let mul r0 r1 =
    if r0.den = 0 || r1.den = 0
    then (r0.num * r1.num) #/ 0
    else q (r0.num * r1.num) (r0.den * r1.den)

  let div r0 r1 =
    if r1.num >= 0
    then mul r0 { num = r1.den; den = r1.num }
    else mul r0 { num = -r1.den; den = -r1.num }

  let inv r = match r.num with
  | 0 -> if r.den = 0 then nan else infinity
  | n when n > 0 -> { num = r.den; den = n }
  | n -> { num = -r.den; den = -n }

  (* Converting *)

  let of_int n = n #/ 1
  let to_int_towards_zero r = r.num / r.den
  let to_int_away_from_zero r =
    if r.num >= 0
    then (r.num + r.den - 1) / r.den
    else -((-r.num + r.den - 1) / r.den)

  let checked_to_int_towards_zero r =
    if r.den = 0 then None else Some (to_int_towards_zero r)

  let checked_to_int_away_from_zero r =
    if r.den = 0 then None else Some (to_int_away_from_zero r)

  let to_float r = float r.num /. float r.den

  (* Formatting *)

  let pp ppf r = pf ppf "%d/%d" r.num r.den
  let pp_kind ppf k = Format.pp_print_string ppf @@ match k with
  | Nan -> "nan" | Neg_infinity -> "-inf" | Infinity -> "inf" | Zero -> "zero"
  | Non_zero -> "non zero"

  (* Operators *)

  let ( = ) r0 r1 = compare r0 r1 = 0
  let ( < ) r0 r1 = compare r0 r1 < 0
  let ( <= ) r0 r1 = compare r0 r1 <= 0
  let ( > ) r0 r1 = compare r0 r1 > 0
  let ( >= ) r0 r1 = compare r0 r1 >= 0
  let ( ~- ) = neg
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
end

(* Music *)

module Pitch = struct
  type abs = int
  type rel = int

  module Abs = struct
    type t = abs
    let midi_min = 0
    let midi_max = 127
    let equal = Int.equal
    let compare = Int.compare
  end

  module Class = struct
    type t =
    [ `Cff | `Cf | `C | `Cs | `Css
    | `Dff | `Df | `D | `Ds | `Dss
    | `Eff | `Ef | `E | `Es | `Ess
    | `Fff | `Ff | `F | `Fs | `Fss
    | `Gff | `Gf | `G | `Gs | `Gss
    | `Aff | `Af | `A | `As | `Ass
    | `Bff | `Bf | `B | `Bs | `Bss ]

    let repr = [| `C; `Cs; `D; `Ds; `E; `F; `Fs; `G; `Gs; `A; `As; `B |]

    let to_int =  function
    | `Cff -> -2 | `Cf -> -1 | `C ->  0 | `Cs ->  1 | `Css ->  2
    | `Dff ->  0 | `Df ->  1 | `D ->  2 | `Ds ->  3 | `Dss ->  4
    | `Eff ->  2 | `Ef ->  3 | `E ->  4 | `Es ->  5 | `Ess ->  6
    | `Fff ->  3 | `Ff ->  4 | `F ->  5 | `Fs ->  6 | `Fss ->  7
    | `Gff ->  5 | `Gf ->  6 | `G ->  7 | `Gs ->  8 | `Gss ->  9
    | `Aff ->  7 | `Af ->  8 | `A ->  9 | `As -> 10 | `Ass -> 11
    | `Bff ->  9 | `Bf -> 10 | `B -> 11 | `Bs -> 12 | `Bss -> 13

    let to_mod_int pc = (to_int pc + 12) mod 12
    let equal = (( = ) : t -> t -> bool)
    let compare pc0 pc1 = Int.compare (to_int pc0) (to_int pc1)
    let mod_equal pc0 pc1 = Int.equal (to_mod_int pc0) (to_mod_int pc1)
    let mod_compare pc0 pc1 = Int.compare (to_mod_int pc0) (to_mod_int pc1)
    let p2 ppf s = pf ppf "@<2>%s" s
    let p3 ppf s = pf ppf "@<3>%s" s
    let pp ppf = function
    | `Cff -> p3 ppf "C♭♭" | `Cf  -> p2 ppf "C♭" | `C -> pf ppf "C"
    | `Cs  -> p2 ppf "C♯"  | `Css -> p3 ppf "C♯♯"
    | `Dff -> p3 ppf "D♭♭" | `Df  -> p2 ppf "D♭" | `D -> pf ppf "D"
    | `Ds  -> p2 ppf "D♯"  | `Dss -> p3 ppf "D♯♯"
    | `Eff -> p3 ppf "E♭♭" | `Ef  -> p2 ppf "E♭" | `E -> pf ppf "E"
    | `Es  -> p2 ppf "E♯"  | `Ess -> p3 ppf "E♯♯"
    | `Fff -> p3 ppf "F♭♭" | `Ff  -> p2 ppf "F♭" | `F -> pf ppf "F"
    | `Fs  -> p2 ppf "F♯"  | `Fss -> p3 ppf "F♯♯"
    | `Gff -> p3 ppf "G♭♭" | `Gf  -> p2 ppf "G♭" | `G -> pf ppf "G"
    | `Gs  -> p2 ppf "G♯"  | `Gss -> p3 ppf "G♯♯"
    | `Aff -> p3 ppf "A♭♭" | `Af  -> p2 ppf "A♭" | `A -> pf ppf "A"
    | `As  -> p2 ppf "A♯"  | `Ass -> p3 ppf "A♯♯"
    | `Bff -> p3 ppf "B♭♭" | `Bf  -> p2 ppf "B♭" | `B -> pf ppf "B"
    | `Bs ->  p2 ppf "B♯"  | `Bss -> p3 ppf "B♯♯"
  end

  type octave = int
  type t = Class.t * octave

  let a440 = (`A, 4)
  let midi_min = (`C, -1)
  let midi_max = (`G, 9)
  let to_abs (pc, o) = 12 * (o + 1) + Class.to_int pc
  let of_abs ap = Class.repr.(ap mod 12), (ap / 12) - 1
  let transp rp p = of_abs (to_abs p + rp)
  let succ p = transp 1 p
  let pred p = transp (-1) p
  let equal p0 p1 = Int.equal (to_abs p0) (to_abs p1)
  let compare p0 p1 = Int.compare (to_abs p0) (to_abs p1)
  let pp ppf (pc, o) = pf ppf "%a%d" Class.pp pc o
end

(* Music *)

module Instrument = struct
  type t =
  [ `Custom of string
  | `Percussion
  | `Accordion | `Acoustic_bass | `Acoustic_grand_piano
  | `Acoustic_guitar_nylon | `Acoustic_guitar_steel | `Agogo | `Alto_sax
  | `Applause | `Bagpipe | `Banjo | `Baritone_sax | `Bassoon | `Bird_tweet
  | `Blown_bottle | `Brass_section | `Breath_noise | `Bright_acoustic_piano
  | `Celesta | `Cello | `Choir_aahs | `Chorused_piano | `Church_organ
  | `Clarinet | `Clavinet | `Contrabass | `Distortion_guitar | `Dulccimer
  | `Electric_bass_fingered | `Electric_bass_picked | `Electric_grand_piano
  | `Electric_guitar_clean | `Electric_guitar_jazz | `Electric_guitar_muted
  | `English_horn | `Fiddle | `Flute | `French_horn | `Fretless_bass
  | `Fx1_train | `Fx2_soundtrack | `Fx3_crystal | `Fx4_atmosphere
  | `Fx5_brightness | `Fx6_goblins | `Fx7_echoes | `Fx8_sci_fi
  | `Glockenspiel | `Guitar_fret_noise | `Guitar_harmonics | `Gunshot
  | `Hammond_organ | `Harmonica | `Harpsichord | `Helicopter
  | `Honky_tonk_piano | `Kalimba | `Koto | `Lead1_square | `Lead2_sawtooth
  | `Lead3_calliope | `Lead4_chiff | `Lead5_charang | `Lead6_voice
  | `Lead7_fifths | `Lead8_bass_lead | `Marimba | `Melodic_drum | `Music_box
  | `Muted_trumpet | `Oboe | `Ocarina | `Orchestra_hit | `Orchestral_harp
  | `Overdriven_guitar | `Pad1_new_age | `Pad2_warm | `Pad3_polysynth
  | `Pad4_choir | `Pad5_bowed | `Pad6_metallic | `Pad7_halo | `Pad8_sweep
  | `Pan_flute | `Percussive_organ | `Piccolo | `Pizzicato_strings
  | `Recorder | `Reed_organ | `Reverse_cymbal | `Rhodes_piano | `Rock_organ
  | `Seashore | `Shakuhachi | `Shamisen | `Shanai | `Sitar | `Slap_bass1
  | `Slap_bass2 | `Soprano_sax | `Steel_drums | `String_ensemble1
  | `String_ensemble2 | `Synth_bass1 | `Synth_bass2 | `Synth_brass1
  | `Synth_brass2 | `Synth_drum | `Synth_strings1 | `Synth_strings2
  | `Synth_voice | `Taiko_drum | `Tango_accordion | `Telephone_ring
  | `Tenor_sax | `Timpani | `Tinkle_bell | `Tremolo_strings | `Trombone
  | `Trumpet | `Tuba | `Tubular_bells | `Vibraphone | `Viola | `Violin
  | `Voice_oohs | `Whistle | `Woodblock | `Xylophone ]

  (* Cf. https://en.wikipedia.org/wiki/General_MIDI#Program_change_events *)
  let midi_program = function
  | `Custom s -> invalid_arg (Printf.sprintf "Custom instrument %S" s)
  | `Percussion -> invalid_arg "Percussion is not a General MIDI instrument"
  | `Acoustic_grand_piano -> 0 | `Bright_acoustic_piano -> 1
  | `Electric_grand_piano -> 2 | `Honky_tonk_piano -> 3 | `Rhodes_piano -> 4
  | `Chorused_piano -> 5 | `Harpsichord -> 6 | `Clavinet -> 7 | `Celesta -> 8
  | `Glockenspiel -> 9 | `Music_box -> 10 | `Vibraphone -> 11 | `Marimba -> 12
  | `Xylophone -> 13 | `Tubular_bells -> 14 | `Dulccimer -> 15
  | `Hammond_organ -> 16 | `Percussive_organ -> 17 | `Rock_organ -> 18
  | `Church_organ -> 19 | `Reed_organ -> 20 | `Accordion -> 21
  | `Harmonica -> 22 | `Tango_accordion -> 23 | `Acoustic_guitar_nylon -> 24
  | `Acoustic_guitar_steel -> 25 | `Electric_guitar_jazz -> 26
  | `Electric_guitar_clean -> 27 | `Electric_guitar_muted -> 28
  | `Overdriven_guitar -> 29 | `Distortion_guitar -> 30
  | `Guitar_harmonics -> 31 | `Acoustic_bass -> 32
  | `Electric_bass_fingered -> 33 | `Electric_bass_picked -> 34
  | `Fretless_bass -> 35 | `Slap_bass1 -> 36 | `Slap_bass2 -> 37
  | `Synth_bass1 -> 38 | `Synth_bass2 -> 39 | `Violin -> 40 | `Viola -> 41
  | `Cello -> 42 | `Contrabass -> 43 | `Tremolo_strings -> 44
  | `Pizzicato_strings -> 45 | `Orchestral_harp -> 46 | `Timpani -> 47
  | `String_ensemble1 -> 48 | `String_ensemble2 -> 49 | `Synth_strings1 -> 50
  | `Synth_strings2 -> 51 | `Choir_aahs -> 52 | `Voice_oohs -> 53
  | `Synth_voice -> 54 | `Orchestra_hit -> 55 | `Trumpet -> 56
  | `Trombone -> 57 | `Tuba -> 58 | `Muted_trumpet -> 59 | `French_horn -> 60
  | `Brass_section -> 61 | `Synth_brass1 -> 62 | `Synth_brass2 -> 63
  | `Soprano_sax -> 64 | `Alto_sax -> 65 | `Tenor_sax -> 66
  | `Baritone_sax -> 67 | `Oboe -> 68 | `English_horn -> 69 | `Bassoon -> 70
  | `Clarinet -> 71 | `Piccolo -> 72 | `Flute -> 73 | `Recorder -> 74
  | `Pan_flute -> 75 | `Blown_bottle -> 76 | `Shakuhachi -> 77 | `Whistle -> 78
  | `Ocarina -> 79 | `Lead1_square -> 80 | `Lead2_sawtooth -> 81
  | `Lead3_calliope -> 82 | `Lead4_chiff -> 83 | `Lead5_charang -> 84
  | `Lead6_voice -> 85 | `Lead7_fifths -> 86 | `Lead8_bass_lead -> 87
  | `Pad1_new_age -> 88 | `Pad2_warm -> 89 | `Pad3_polysynth -> 90
  | `Pad4_choir -> 91 | `Pad5_bowed -> 92 | `Pad6_metallic -> 93
  | `Pad7_halo -> 94 | `Pad8_sweep -> 95 | `Fx1_train -> 96
  | `Fx2_soundtrack -> 97 | `Fx3_crystal -> 98 | `Fx4_atmosphere -> 99
  | `Fx5_brightness -> 100 | `Fx6_goblins -> 101 | `Fx7_echoes -> 102
  | `Fx8_sci_fi -> 103 | `Sitar -> 104 | `Banjo -> 105 | `Shamisen -> 106
  | `Koto -> 107 | `Kalimba -> 108 | `Bagpipe -> 109 | `Fiddle -> 110
  | `Shanai -> 111 | `Tinkle_bell -> 112 | `Agogo -> 113 | `Steel_drums -> 114
  | `Woodblock -> 115 | `Taiko_drum -> 116 | `Melodic_drum -> 117
  | `Synth_drum -> 118 | `Reverse_cymbal -> 119 | `Guitar_fret_noise -> 120
  | `Breath_noise -> 121 | `Seashore -> 122 | `Bird_tweet -> 123
  | `Telephone_ring -> 124 | `Helicopter -> 125 | `Applause -> 126
  | `Gunshot -> 127

  let equal = (( = ) : t -> t -> bool)
  let compare = (Stdlib.compare : t -> t -> int)

  module T = struct
    type nonrec t = t
    let compare = compare
  end

  module Set = Set.Make (T)
  module Map = struct
    type key = t
    include (Map.Make (T) : Map.S with type key := t)

    let dom m = fold (fun k _ acc -> Set.add k acc) m Set.empty
    let of_list bs = List.fold_left (fun m (k,v) -> add k v m) empty bs

    let add_to_list k v m = match find k m with
    | exception Not_found -> add k [v] m
    | l -> add k (v :: l) m

    let add_to_set
        (type set) (type elt)
        (module S : Stdlib.Set.S with type elt = elt and type t = set)
        k v m
      =
      match find k m with
      | exception Not_found -> add k (S.singleton v) m
      | set -> add k (S.add v set) m
  end
end

module Percussion = struct
  type t =
  [ `Acoustic_bass_drum | `Acoustic_snare | `Bass_drum1 | `Cabasa
  | `Chinese_cymbal | `Claves | `Closed_hi_hat | `Cowbell | `Crash_cymbal1
  | `Crash_cymbal2 | `Electric_snare | `Hand_clap | `Hi_bongo | `Hi_mid_tom
  | `Hi_wood_block | `High_agogo | `High_floor_tom | `High_timbale | `High_tom
  | `Long_guiro | `Long_whistle | `Low_agogo | `Low_bongo | `Low_conga
  | `Low_floor_tom | `Low_mid_tom | `Low_timbale | `Low_tom | `Low_wood_block
  | `Maracas | `Mute_cuica | `Mute_hi_conga | `Mute_triangle | `Open_cuica
  | `Open_hi_conga | `Open_hi_hat | `Open_triangle | `Pedal_hi_hat | `Ride_bell
  | `Ride_cymbal1 | `Ride_cymbal2 | `Short_guiro | `Short_whistle
  | `Side_stick | `Splash_cymbal | `Tambourine | `Vibraslap ]

  (* Sounds as pitches, https://en.wikipedia.org/wiki/General_MIDI#Percussion *)
  let to_abs_pitch = function
  | `Acoustic_bass_drum -> 35 | `Bass_drum1 -> 36 | `Side_stick -> 37
  | `Acoustic_snare -> 38 | `Hand_clap -> 39 | `Electric_snare -> 40
  | `Low_floor_tom -> 41 | `Closed_hi_hat -> 42 | `High_floor_tom -> 43
  | `Pedal_hi_hat -> 44 | `Low_tom -> 45 | `Open_hi_hat -> 46
  | `Low_mid_tom -> 47 | `Hi_mid_tom -> 48 | `Crash_cymbal1 -> 49
  | `High_tom -> 50 | `Ride_cymbal1 -> 51 | `Chinese_cymbal -> 52
  | `Ride_bell -> 53 | `Tambourine -> 54 | `Splash_cymbal -> 55
  | `Cowbell -> 56 | `Crash_cymbal2 -> 57 | `Vibraslap -> 58
  | `Ride_cymbal2 -> 59 | `Hi_bongo -> 60 | `Low_bongo -> 61
  | `Mute_hi_conga -> 62 | `Open_hi_conga -> 63 | `Low_conga -> 64
  | `High_timbale -> 65 | `Low_timbale -> 66 | `High_agogo -> 67
  | `Low_agogo -> 68 | `Cabasa -> 69 | `Maracas -> 70 | `Short_whistle -> 71
  | `Long_whistle -> 72 | `Short_guiro -> 73 | `Long_guiro -> 74 | `Claves -> 75
  | `Hi_wood_block -> 76 | `Low_wood_block -> 77 | `Mute_cuica -> 78
  | `Open_cuica -> 79 | `Mute_triangle -> 80 | `Open_triangle -> 81

  let midi_min = 35
  let midi_max = 81
  let to_pitch snd = Pitch.of_abs (to_abs_pitch snd)
  let equal = (( = ) : t -> t -> bool)
  let compare = (Stdlib.compare : t -> t -> int)
end

module Music = struct
  type dur = Q.t
  module Dur = struct
    type t = dur
    let equal = Q.equal
    let compare = Q.compare
    let pp = Q.pp
  end

  module Mode = struct
    type t =
    [ `Major | `Minor | `Ionian | `Dorian | `Phrygian | `Lydian | `Myxolydian
    | `Aeolian | `Locrian | `Custom of string ]

    let major_intervals = [2; 2; 1; 2; 2; 2]
    let minor_intervals = [2; 1; 2; 2; 1; 2]
    let mode_to_intervals = function
    | `Major -> major_intervals
    | `Minor -> minor_intervals
    | `Ionian -> major_intervals
    | `Dorian -> [2; 1; 2; 2; 2; 1]
    | `Phrygian -> [1; 2; 2; 2; 1; 2]
    | `Lydian -> [2; 2; 2; 1; 2; 2]
    | `Myxolydian -> [2; 2; 1; 2; 2; 1]
    | `Aeolian -> minor_intervals
    | `Locrian -> [1; 2; 2; 1; 2; 2]
    | `Custom _ -> []
  end

  module Phrase = struct
    type loudness =
    [ `Ppp | `Pp | `P | `Mp | `Mf | `F | `Ff | `Fff | `Volume of int ]

    type dynamic =
    [ `Accent of Q.t | `Crescendo of Q.t | `Diminuendo of Q.t
    | `Loudness of loudness ]

    type tempo = [ `Ritardando of Q.t | `Accelerando of Q.t ]

    type articulation =
    [ `Staccato of Q.t | `Legato of Q.t | `Slurred of Q.t
    | `Tenuto | `Marcato | `Pedal | `Fermata | `Fermata_down | `Breath
    | `Down_bow | `Up_bow | `Harmonic | `Pizzicato | `Left_pizz | `Bartok_pizz
    | `Swell | `Wedge | `Thumb | `Stopped ]

    type note_head =
    [ `Art_harmonic | `Diamond | `None | `Slash | `Square | `Tremolo | `Triangle
    | `X ]

    type ornament =
    [ `Trill | `Mordent | `Inv_mordent | `Double_mordent
    | `Turn | `Trilled_turn | `Short_trill
    | `Arpeggio | `Arpeggio_up | `Arpeggio_down
    | `Instruction of string | `Head of note_head
    | `Diatonic_trans of int ]

    type attr =
    [ `Dyn of dynamic | `Tempo of tempo | `Art of articulation
    | `Orn of ornament ]

    type t = attr list
  end

  module Ctrl = struct
    type t =
    [ `Tempo of Q.t
    | `Transp of Pitch.rel
    | `Instrument of Instrument.t
    | `Phrase of Phrase.attr list
    | `Player of string
    | `Key_sig of Pitch.Class.t * Mode.t ]
  end

  module Prim = struct
    type 'a t = [ `Note of Dur.t * 'a | `Rest of Dur.t ]
    let note d a = `Note (d, a)
    let rest d = `Rest d
    let dur = function `Note (d, _) -> d | `Rest d -> d
    let fold ~note ~rest = function `Note (d, a) -> note d a | `Rest d -> rest d
    let map f = function `Note (d, p) -> `Note (d, f p) | `Rest _ as v -> v
    let map_dur f = function
    | `Note (d, p) -> `Note (f d, p) | `Rest d -> `Rest (f d)
  end

  (* Music *)

  type 'a t =
    [ `Prim of 'a Prim.t
    | `Seq of 'a t * 'a t
    | `Par of 'a t * 'a t
    | `Ctrl of Ctrl.t * 'a t
 (* | `Lazy of 'a t lazy *)
    ]

  let prim p = `Prim p
  let seq m0 m1 = `Seq (m0, m1)
  let par m0 m1 = `Par (m0, m1)
  let ctrl c m = `Ctrl (c, m)

  (* Traversals *)

  let rec map f = function
  | `Prim p -> `Prim (Prim.map f p)
  | `Seq (m0, m1) -> `Seq (map f m0, map f m1)
  | `Par (m0, m1) -> `Par (map f m0, map f m1)
  | `Ctrl (c, m) -> `Ctrl (c, map f m)

  let rec map_prims f = function
  | `Prim p -> `Prim (f p)
  | `Seq (m0, m1) -> `Seq (map_prims f m0, map_prims f m1)
  | `Par (m0, m1) -> `Par (map_prims f m0, map_prims f m1)
  | `Ctrl (c, m) -> `Ctrl (c, map_prims f m)

  let rec fold ~prim ~seq ~par ~ctrl = function
  | `Prim p ->
      prim p
  | `Seq (m0, m1) ->
      seq (fold ~prim ~seq ~par ~ctrl m0) (fold ~prim ~seq ~par ~ctrl m1)
  | `Par (m0, m1) ->
      par (fold ~prim ~seq ~par ~ctrl m0) (fold ~prim ~seq ~par ~ctrl m1)
  | `Ctrl (c, m) ->
      ctrl c (fold ~prim ~seq ~par ~ctrl m)

  (* Properties *)

  let rec dur = function
  | `Prim p -> Prim.dur p
  | `Seq (m0, m1) -> Q.add (dur m0) (dur m1)
  | `Par (m0, m1) -> Q.max (dur m0) (dur m1)
  | `Ctrl (`Tempo r, m) -> Q.div (dur m) r
  | `Ctrl (_, m) -> dur m
end

module Syntax = struct
  let ( ^ ) = Music.seq
  let ( @|@ ) = Music.par
end

module M = struct

  (* Basic music *)

  let note d p = `Prim (`Note (d, p))
  let rest d = `Prim (`Rest d)
  let seq = Music.seq
  let par = Music.par
  let ctrl = Music.ctrl
  let nil = `Prim (`Rest Q.zero)
  let line ms = List.fold_right seq ms nil
  let chord ms = List.fold_right par ms nil
  let rec times n m = match n with
  | n when n < 0 -> invalid_arg (strf "negative repetition (%d)" n)
  | 0 -> nil
  | n -> seq m (times (n - 1) m)

  let rec retro = function
  | `Prim _ as m -> m
  | `Ctrl (c, m) -> `Ctrl (c, retro m)
  | `Seq (m0, m1) -> `Seq (retro m1, retro m0)
  | `Par (m0, m1) ->
      let d0 = Music.dur m0 in
      let d1 = Music.dur m1 in
      if Q.(d0 > d1)
      then par (retro m0) (seq (rest Q.(d0 - d1)) (retro m1))
      else par (seq (rest Q.(d1 - d0)) (retro m0)) (retro m1)

  let rec drop_zeros m =
    let try_drop m0 m1 cons = match m0, m1 with
    | `Prim (`Note (d, _)), m when Q.(d = zero) -> m
    | `Prim (`Rest d), m when Q.(d = zero) -> m
    | m, `Prim (`Note (d, _)) when Q.(d = zero) -> m
    | m, `Prim (`Rest d) when Q.(d = zero) -> m
    | m0, m1 -> cons m0 m1
    in
    match m with
    | `Prim _ as p -> p
    | `Seq (m0, m1) -> try_drop (drop_zeros m0) (drop_zeros m1) seq
    | `Par (m0, m1) -> try_drop (drop_zeros m0) (drop_zeros m1) par
    | `Ctrl (c, m) -> ctrl c (drop_zeros m)

  (* Ornaments *)

  let grace rd rp m =
    let d, p = match m with
    | `Prim (`Note (d, p)) -> d, p
    | _ -> invalid_arg "Not a `Prim (`Note _) value"
    in
    let g = note Q.(rd * d) (Pitch.transp rp p) in
    let n = note Q.((one - rd) * d) p in
    seq g n

  (* Instruments and percussion *)

  let perc snd dur =
    `Ctrl (`Instrument `Percussion, note dur (Percussion.to_pitch snd))

  let instrument i m =`Ctrl (`Instrument i, m)
  let rec drop_instruments = function
  | `Ctrl (`Instrument i, m) -> drop_instruments m
  | `Ctrl (c, m) -> `Ctrl (c, drop_instruments m)
  | `Seq (m0, m1) -> `Seq (drop_instruments m0, drop_instruments m1)
  | `Par (m0, m1) -> `Par (drop_instruments m0, drop_instruments m1)
  | `Prim _ as m -> m

  let change_instrument i m = `Ctrl (`Instrument i, drop_instruments m)

  (* Other controls *)

  let tempo d m = `Ctrl (`Tempo d, m)
  let transp rp m = `Ctrl (`Transp rp, m)
  let phrase ps m = `Ctrl (`Phrase ps, m)
  let key_sig pc mode m = `Ctrl (`Key_sig (pc, mode), m)

  (* Breaking *)

  let rec take d = function
  | (m : 'a Music.t) when Q.(d <= zero) -> nil
  | `Prim (`Note (nd, p)) -> note Q.(min d nd) p
  | `Prim (`Rest rd) -> rest Q.(min d rd)
  | `Par (m0, m1) -> par (take d m0) (take d m1)
  | `Seq (m0, m1) ->
      let m0' = take d m0 in
      let m1' = take Q.(d - Music.dur m0') m1 in
      seq m0' m1'
  | `Ctrl (`Tempo r, m) -> tempo r (take Q.(d * r) m)
  | `Ctrl (c, m) -> ctrl c (take d m)

  let rec drop d = function
  | (m : 'a Music.t) when Q.(d <= zero) -> m
  | `Prim (`Note (nd, p)) -> note Q.(max (nd - d) zero) p
  | `Prim (`Rest rd) -> rest Q.(max (rd - d) zero)
  | `Par (m0, m1) -> par (drop d m0) (drop d m1)
  | `Seq (m0, m1) ->
      let m0' = drop d m0 in
      let m1' = drop Q.(d - Music.dur m0) m1 in
      seq m0' m1'
  | `Ctrl (`Tempo r, m) -> tempo r (drop Q.(d * r) m)
  | `Ctrl (c, m) -> ctrl c (drop d m)

  (* Timing *)

  let offset d m = Music.seq (rest d) m
  let mul_durs r m = Music.map_prims (Music.Prim.map_dur (Q.mul r)) m

  let bn = Q.(2 #/ 1)   let wn = Q.(1 #/ 1)
  let hn = Q.(1 #/ 2)   let qn = Q.(1 #/ 4)
  let en = Q.(1 #/ 8)   let sn = Q.(1 #/ 16)
  let tsn = Q.(1 #/ 32) let sfn = Q.(1 #/ 64)
  let dwn = Q.(3 #/ 2)  let dhn = Q.(3 #/ 4)
  let dqn = Q.(3 #/ 8)  let den = Q.(3 #/ 16)
  let dsn = Q.(3 #/ 32) let dtsn = Q.(3 #/ 64)
  let ddhn = Q.(7 #/ 8) let ddqn = Q.(7 #/ 16)
  let dden = Q.(7 #/ 32)

  let bnr = rest bn     let wnr = rest wn
  let hnr = rest hn     let qnr = rest qn
  let enr = rest en     let snr = rest sn
  let tsnr = rest tsn   let sfnr = rest sfn
  let dwnr = rest dwn   let dhnr = rest dhn
  let dqnr = rest dqn   let denr = rest den
  let dsnr = rest dsn   let dtsnr = rest dtsn
  let ddhnr = rest ddhn let ddqnr = rest ddqn
  let ddenr = rest dden

  (* Pitched notes *)

  let cff o d = note d (`Cff, o) let cf o d = note d (`Cf, o)
  let c o d = note d (`C, o)     let dff o d = note d (`Dff, o)
  let cs o d = note d (`Cs, o)   let df o d = note d (`Df, o)
  let css o d = note d (`Css, o) let d o d = note d (`D, o)
  let eff o d = note d (`Eff, o) let ds o d = note d (`Ds, o)
  let ef o d = note d (`Ef, o)   let fff o d = note d (`Fff, o)
  let dss o d = note d (`Dss, o) let e o d = note d (`E, o)
  let ff o d = note d (`Ff, o)   let es o d = note d (`Es, o)
  let f o d = note d (`F, o)     let gff o d = note d (`Gff, o)
  let ess o d = note d (`Ess, o) let fs o d = note d (`Fs, o)
  let gf o d = note d (`Gf, o)   let fss o d = note d (`Fss, o)
  let g o d = note d (`G, o)     let aff o d = note d (`Aff, o)
  let gs o d = note d (`Gs, o)   let af o d = note d (`Af, o)
  let gss o d = note d (`Gss, o) let a o d = note d (`A, o)
  let bff o d = note d (`Bff, o) let as' o d = note d (`As, o)
  let bf o d = note d (`Bf, o)   let ass o d = note d (`Ass, o)
  let b o d = note d (`B, o)     let bs o d = note d (`Bs, o)
  let bss o d = note d (`Bss, o)

  (* Operators *)

  include Syntax
end

(* Notes and performances *)

module Pnote = struct
  type volume = int
  type attr =
  [ `Volume of volume
  | `Fingering of int
  | `Dynamics of int
  | `Params of float list ]

  type t = Pitch.t * attr list

  let transp rp (p, atts) = (Pitch.transp rp p, atts)
  let of_pitch p = p, []
  let of_pitch_volume (p, v) = p, [`Volume v]
  let of_abs_pitch ap = Pitch.of_abs ap, []
  let of_abs_pitch_volume (ap, v) = Pitch.of_abs ap, [`Volume v]
end

module Performance = struct
  type volume = int
  type time_s = Q.t
  type dur_s = Q.t
  let wn_dur_s ~bpm ~beat_dur = Q.(of_int 60 / (of_int bpm * beat_dur))

  module Event = struct
    type t =
      { time_s : time_s;
        instrument : Instrument.t;
        pitch : Pitch.abs;
        dur_s : dur_s;
        volume : volume;
        params : float list; }

    let v time_s instrument pitch dur_s volume params =
      { time_s; instrument; pitch; dur_s; volume; params }

    let time_s t = t.time_s
    let instrument t = t.instrument
    let pitch t = t.pitch
    let dur_s t = t.dur_s
    let volume t = t.volume
    let params t = t.params

    let scale_dur_s dr e = { e with dur_s = Q.(dr * e.dur_s) }
    let order_by_time e0 e1 = Q.compare e0.time_s e1.time_s
  end

  module Ctx = struct
    type 'a player = unit (* TODO interprets phrases and notes *)
    type 'a t =
      { time_s : time_s;
        wn_dur_s : dur_s; (* Duration of M.wn (Q.one) *)
        instrument : Instrument.t;
        player : 'a player;
        transp : Pitch.rel; (* Pitch transposer (`Ctrl transp) *)
        volume : Pnote.volume }

    let default_wn_dur = wn_dur_s ~bpm:120 ~beat_dur:M.qn
    let default =
      let time_s = Q.zero and wn_dur_s = default_wn_dur
      and instrument = `Acoustic_grand_piano and transp = 0 and volume = 127
      and player = () in
      { time_s; wn_dur_s; instrument; player; transp; volume }

    let v ?(init = default) ?(time_s = init.time_s) ?(wn_dur_s = init.wn_dur_s)
        ?(instrument = init.instrument) ?(player = init.player)
        ?(transp = init.transp) ?(volume = init.volume) () =
      { time_s; wn_dur_s; instrument; player; transp; volume }

    let time_s c = c.time_s
    let wn_dur_s c = c.wn_dur_s
    let instrument c = c.instrument
    let player c = c.player
    let transp c = c.transp
    let volume c = c.volume
  end

  (* Performances *)

  type t = Event.t list

  let empty = []

  let rec merge p0 p1 : t = match p0, p1 with
  | [], p | p, [] -> p
  | e0 :: es0, e1 :: es1 ->
      if Q.(e0.time_s < e1.time_s)
      then e0 :: merge es0 p1
      else e1 :: merge p0 es1

  let by_instrument p =
    let add acc e = Instrument.Map.add_to_list (Event.instrument e) e acc in
    let rev_by = List.fold_left add Instrument.Map.empty p in
    Instrument.Map.map List.rev rev_by

  (* Converting *)

  let dur_to_s c dur = Q.(dur * c.Ctx.wn_dur_s)

  let pnote_to_event c dur (p, atts) =
    let rec loop c volume params = function
    | [] ->
        let time_s = c.Ctx.time_s and instrument = c.instrument
        and pitch = Pitch.to_abs p + c.transp and dur_s = dur_to_s c dur in
        { Event.time_s; instrument; pitch; dur_s; volume; params }
    | `Volume volume :: atts -> loop c volume params atts
    | `Params params :: atts -> loop c volume params atts
    | _ :: atts -> loop c volume params atts
    in
    loop c c.volume [] atts

  let prim_to_events c = function
  | `Rest dur -> [], dur_to_s c dur
  | `Note (dur, n) ->
      let e = pnote_to_event c dur n in
      let d = Event.dur_s e in
      if Q.(d = zero) then [], Q.zero else [e], Event.dur_s e

  let rec phrase_to_events c (ps : Music.Phrase.t) m =
    let loudness_to_volume = function
    | `Ppp -> 16 | `Pp -> 33 | `P -> 49 | `Mp -> 64 | `Mf -> 80
    | `F -> 96 | `Ff -> 112 | `Fff -> 127 | `Volume v -> v
    in
    match ps with
    | [] -> music_to_events c m
    | `Dyn (`Loudness l) :: ps ->
        phrase_to_events { c with Ctx.volume = loudness_to_volume l} ps m
    | `Art (`Staccato dr | `Legato dr) :: ps ->
        let p, d = phrase_to_events c ps m in
        let p' = List.map (Event.scale_dur_s dr) p in
        p', d
    | `Art (`Slurred dr) :: ps ->
        let p, d = phrase_to_events c ps m in
        let max_time t e = Q.(max t (Event.time_s e)) in
        let last_start = List.fold_left max_time Q.zero p in
        let set_dur dr e =
          if Event.time_s e < last_start then Event.scale_dur_s dr e else e
        in
        let p' = List.map (set_dur dr) p in
        p', d
    | _ :: atts -> phrase_to_events c ps m

  and music_to_events c = function
  | `Prim p -> prim_to_events c p
  | `Seq (m0, m1) ->
      let p0, d0 = music_to_events c m0 in
      let c = { c with Ctx.time_s = Q.(c.time_s + d0) } in
      let p1, d1 = music_to_events c m1 in
      p0 @ p1, Q.(d0 + d1)
  | `Par (m0, m1) ->
      let p0, d0 = music_to_events c m0 in
      let p1, d1 = music_to_events c m1 in
      merge p0 p1, Q.max d0 d1
  | `Ctrl (`Instrument instrument, m) ->
      music_to_events { c with Ctx.instrument } m
  | `Ctrl (`Tempo t, m) ->
      music_to_events { c with Ctx.wn_dur_s = Q.(c.wn_dur_s / t) } m
  | `Ctrl (`Transp dp, m) ->
      music_to_events { c with Ctx.transp = c.transp + dp } m
  | `Ctrl (`Phrase ps, m) -> phrase_to_events c ps m
  | `Ctrl (`Key_sig _, m) -> (* no effect *) music_to_events c m
  | `Ctrl (`Player _, m) -> (* TODO *) music_to_events c m

  let of_music ?(ctx = Ctx.default) m = fst (music_to_events ctx m)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2021 The mu programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
