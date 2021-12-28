(*---------------------------------------------------------------------------
   Copyright (c) 2021 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Declarative music.

    See an {{!example}example}.

    Open this module to use it, it defines only modules in your scope.
    See also {!Mu.Syntax}.

    {b References.}

    The music model is taken from.
    {ul
    {- Paul Hudak and Donya Quick.
    {{:https://doi.org/10.1017/9781108241861}The Haskell School of Music}.
    2018.}} *)

(** {1:rat Rational numbers} *)

(** Rational numbers (ℚ). *)
module Q : sig

  (** {1:rationals Rational numbers} *)

  type t
  (** The type for rational numbers.

      Values of this type are normalized. The numerator and denominator
      have no common factor and the denominator is non-negative.

      The special rationals [1/0], [-1/0] and [0/0] repectively denote
      [infinity], [neg_infinity] and [nan]. *)

  val v : int -> int -> t
  (** [v num den] is the normalized rational [num / den]. *)

  val int : int -> t
  (** [int n] is [v n 1]. *)

  val ( #/ ) : int -> int -> t
  (** [n #/ d]  is [v num den]. *)

  val num : t -> int
  (** [num r] is the numerator of [r]. *)

  val den : t -> int
  (** [den r] is the denominator of [r]. *)

  (** {1:constants Constants} *)

  val zero : t
  (** [zero] is [0/1]. *)

  val one : t
  (** [one] is [1/1]. *)

  val minus_one : t
  (** [minus_one] is [-1/1]. *)

  val infinity : t
  (** [infinity] is [1/0]. *)

  val neg_infinity : t
  (** [neg_infinity] is [-1/0]. *)

  val nan : t
  (** [nan] is [0/0]. *)

  (** {1:preds Predicates and comparisons} *)

  type kind = Nan | Neg_infinity | Infinity | Zero | Non_zero (** *)
  (** The type for kinds of rational numbers. *)

  val kind : t -> kind
  (** [kind r] is the kind of [r]. *)

  val is_finite : t -> bool
  (** [is_finite r] is [true] iff [r]'s kind is either [Zero] or
      [Non_zero]. *)

  val is_infinite : t -> bool
  (** [is_infinite r] is [true] iff [r] is either {!infinity} or
      {!neg_infinity}. *)

  val is_nan : t -> bool
  (** [is_nen r] is [true] iff [r] is {!nan}. *)

  val sign : t -> int
  (** [sign r] is the sign of [r]. This is either [-1], [0] or [1]. The sign
      of [Nan] is [0]. *)

  val compare : t -> t -> int
  (** [compare r0 r1] is a total order on [r0] and [r1]. {!nan} is the
      smallest value and equal to itself, the rest is ordered as
      expected. *)

  val equal : t -> t -> bool
  (** [equal r0 r1] is [compare r0 r1 = 0]. [nan] is equal to itself. *)

  val ( = ) : t -> t -> bool
  (** [r0 = r1] is [compare r0 r1 = 0]. *)

  val ( < ) : t -> t -> bool
  (** [r0 < r1] is [compare r0 r1 < 0]. *)

  val ( <= ) : t -> t -> bool
  (** [r0 <= r1] is [compare r0 r1 <= 0]. *)

  val ( > ) : t -> t -> bool
  (** [r0 > r1] is [compare r0 r1 > 0]. *)

  val ( >= ) : t -> t -> bool
  (** [r0 >= r1] is [compare r0 r1 >= 0]. *)

  val min : t -> t -> t
  (** [min r0 r1] is the smallest of [r0] and [r1]. *)

  val max : t -> t -> t
  (** [max r0 r1] is the largest of [r0] and [r1]. *)

  (** {1:arith Arithmetic}

      {b Note.} All these operations return [nan] if they get a [nan]
      argument. *)

  val neg : t -> t
  (** [neg r] is [r] negated. *)

  val abs : t -> t
  (** [abs r] is [r]'s absolute value. *)

  val add : t -> t -> t
  (** [add r0 r1] is [r1] added to [r0]. *)

  val sub : t -> t -> t
  (** [sub r0 r1] is [r1] subtracted from [r0]. *)

  val mul : t -> t -> t
  (** [mul r0 r1] is [r0] multiplied by [r1]. *)

  val inv : t -> t
  (** [inv r] is the inverse of [r]. *)

  val div : t -> t -> t
  (** [div r0 r1] is [r0] divided by [r1]. *)

  val ( ~- ) : t -> t
  (** [~- r] is [neg r]. *)

  val ( + ) : t -> t -> t
  (** [r0 + r1] is [add r0 r1]. *)

  val ( - ) : t -> t -> t
  (** [r0 - r1] is [sub r0 r1]. *)

  val ( * ) : t -> t -> t
  (** [r0 * r1] is [mul r0 r1]. *)

  val ( / ) : t -> t -> t
  (** [r0 / r1] is [div r0 r1]. *)

  (** {1:convert Converting} *)

  val of_int : int -> t
  (** [of_int n] is [n // 1]. *)

  val to_int : t -> int
  (** [to_int r] is [num r / den r]. Raises [Division_by_zero] if [den r]
      is [0]; that is on {!nan}, {!neg_infinity}, {!infinity}. *)

  val safe_to_int : t -> int option
  (** [safe_to_int] is like {!to_int} but is [None] if [den r] is [0]. *)

  val to_float : t -> float
  (** [to_float r] is [(float (num r) /. (float (den r))]. Special values
      are mapped to corresponding floating point special values. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] is a formatter for rationals. *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind] is a formatter for rational kinds. *)
end

(** {1:music Music} *)

(** Pitches. *)
module Pitch : sig

  (** {1:abs Absolute pitch} *)

  type abs = int
  (** The type for absolute pitches in semitones on a chromatic
      12-tone scale. The absolute pitch of
      {{:https://en.wikipedia.org/wiki/C_(musical_note)#Middle_C}
      middle C} (C{_4}) is [60].  Constrained to the range
      \[[0];[127]\] these values correspond to MIDI notes. *)

  type rel = int
  (** The type for relative pitches. This is an absolute pitch
      difference. *)

  (** Absolute pitches. *)
  module Abs : sig

    (** {1:abs Absolute pitches} *)

    type t = abs
    (** See {!type-abs}. *)

    val midi_min : abs
    (** [midi_min] is [0], the lowest MIDI note. *)

    val midi_max : abs
    (** [midi_max] is [127], the highest MIDI note. *)

    (** {1:preds Predicates and comparisons} *)

    val equal : abs -> abs -> bool
    (** [equal ap0 ap1] is [true] if [ap0] and [ap1] are the same absolute
        pitch. *)

    val compare : abs -> abs -> int
    (** [compare] is a total order on absolute pitches compatible with
        [equal]. *)
  end

  (** {1:pitch_classes Pitch classes} *)

  (** Pitch classes.

      In the naming scheme, [f] stands for {e flat} (♭) and [s] is for
      {e sharp} (♯). *)
  module Class : sig

    (** {1:pitch_classes Pitch classes} *)

    type t =
    [ `Cff (** C{^♭♭} *) | `Cf (** C{^♭} *) | `C (** C *) | `Cs (** C{^♯} *)
    | `Css (** C{^♯♯} *)
    | `Dff (** D{^♭♭} *) | `Df (** D{^♭} *) | `D (** D *) | `Ds (** D{^♯} *)
    | `Dss (** D{^♯♯} *)
    | `Eff (** E{^♭♭} *) | `Ef (** E{^♭} *) | `E (** E *) | `Es (** E{^♯} *)
    | `Ess (** E{^♯♯} *)
    | `Fff (** F{^♭♭} *) | `Ff (** F{^♭} *) | `F (** F *) | `Fs (** F{^♯} *)
    | `Fss (** F{^♯♯} *)
    | `Gff (** G{^♭♭} *) | `Gf (** G{^♭} *) | `G (** G *) | `Gs (** G{^♯} *)
    | `Gss (** G{^♯♯} *)
    | `Aff (** F{^♭♭} *) | `Af (** A{^♭} *) | `A (** A *) | `As (** A{^♯} *)
    | `Ass (** A{^♯♯} *)
    | `Bff (** B{^♭♭} *) | `Bf (** B{^♭} *) | `B (** B *) | `Bs (** B{^♯} *)
    | `Bss (** B{^♯♯} *) ]
    (** The type for pitch classes. The representation is not unique,
        for example [Css] (C{^♯♯}) and [D] are the same pitch class. *)

    val to_int : t -> int
    (** [to_int c] is [c] as an integer on a non-modular semitone scale.
        [`C] is 0, [`Cff] is [-2] and [`Bss] is [13]. See also
        {!to_mod_int}. *)

    val to_mod_int : t -> int
    (** [to_mod_int c] is like {!to_int} but the result is non-negative
        and modulo [12], [`C] is [0]. Enharmonics like [`C] and [`Bs]
        are mapped to the same integer. See also {!to_int}. *)

    (** {1:preds Predicates and comparisons}

        {b XXX.} Should the default [equal] and [compare] be the modular
        one ? *)

    val equal : t -> t -> bool
    (** [equal c0 c1] is [true] iff [to_int c0 = to_int c1]. Note that
        this does not equate enharmonics like [`C] and [`Bs], use
        {!mod_equal} for that. *)

    val compare : t -> t -> int
    (** [compare c0 c1] orders [c0] and [c1] according to
        {!to_int}. Note that this does not equate enharmonics like [`C]
        and [`Bs], use {!mod_compare} for that. *)

    val mod_equal : t -> t -> bool
    (** [mod_equal c0 c1] is [true] iff [c0] and [c1] represent the same
        pitch class. Equates enharmonics like [`C] and [`Bs]. *)

    val mod_compare : t -> t -> int
    (** [mod_compare c0 c1] is a total order on pitches compatible with
        {!mod_equal}. *)

    (** {1:fmt Formatters} *)

    val pp : Format.formatter -> t -> unit
    (** [pp] formats pitch classes. Uses the Unicode ♭ and ♯ characters. *)
  end

  (** {1:pitches Pitches} *)

  type octave = int
  (** The type for octave numbers. Octave number [4] corresponds to the
      octave of
      {{:https://en.wikipedia.org/wiki/C_(musical_note)#Middle_C}middle
      C} (C{_4}). *)

  type t = Class.t * octave
  (** The type for pitches. A pitch class and an octave.
      {ul
      {- [(`A, 4)] is {{:https://en.wikipedia.org/wiki/A440_(pitch_standard)}
         A440}.}
      {- [(`A, 0)] to [(`C, 8)] is the range of a piano.}
      {- [(`C, -1)] to [(`G, 9)] is the the MIDI note range.}} *)

  val a440 : t
  (** [a440] is [(`A, 4)], the standard pitch
      {{:https://en.wikipedia.org/wiki/A440_(pitch_standard)}A440}. *)

  val midi_min : t
  (** [midi_min] is [(`C, -1)], the lowest MIDI note. *)

  val midi_max : t
  (** [midi_max] is [(`G, 9)], the highest MIDI note. *)

  (** {1:ops Operations} *)

  val transp : rel -> t -> t
  (** [transp r p] transposes pitch [p] by [r] relative semitones. *)

  val succ : t -> t
  (** [succ p] is [transp 1 p]. *)

  val pred : t -> t
  (** [pred p] is [transp (-1) p]. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal p0 p1] is [true] iff [p0] and [p1] have the same absolute pitch. *)

  val compare : t -> t -> int
  (** [compare p0 p1] is a total order on pitches compatible with [equal]. *)

  (** {1:convert Converting} *)

  val to_abs : t -> abs
  (** [to_abs p] is the absolute pitch of [p]. *)

  val of_abs : abs -> t
  (** [of_abs ap] is {e a} pitch for the absolute pitch of [ap]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats pitches. *)
end

(** Music instruments. *)
module Instrument : sig

  (** {1:instruments Instruments} *)

  type t =
  [ `Custom of string (** Not in General MIDI *)
  | `Percussion (** Not in General MIDI, use with {!Percussion}. *)
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
  (** The type for instruments as per General MIDI
      {{:https://en.wikipedia.org/wiki/General_MIDI#Program_change_events}
      programs} except for [`Custom] and [`Percussion].

      [`Percussion] should be used with {!Percussion} pitches, these
      are treated specially during MIDI. They are performed on channel 10. *)

  val midi_program : t -> int
  (** [midi_program i] is the zero-based MIDI program number for
      instrument [i].  Raises [Invalid_argument] on [`Custom _] or
      [`Percussion]. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal i0 i1] is [true] iff [i0] and [i1] are the same instrument. *)

  val compare : t -> t -> int
  (** [compare] is a total order on instruments compatible with [equal]. *)

  (** {1:maps Instrument sets and maps} *)

  (** Instrument sets. *)
  module Set : sig

    (** {1:sets Instrument sets} *)

    include Set.S with type elt = t
  end

  (** Instrument maps. *)
  module Map : sig

    (** {1:maps Instrument maps} *)

    type key = t

    include Map.S with type key := t

    (** {1:adds Additionals adds} *)

    val add_to_list : key -> 'a -> 'a list t -> 'a list t
    (** [add k v m] is [m] with [k] mapping to [l] such that [l] is
        [v :: find k m] if [k] was bound in [m] and [[v]] otherwise. *)

    val add_to_set :
      (module Stdlib.Set.S with type elt = 'a and type t = 'set) ->
      key -> 'a -> 'set t -> 'set t
      (** [add (module S) k v m] is [m] with [k] mapping to [s] such that [s] is
          [S.add v (find k m)] if [k] was bound in [m] and [S.singleton [v]]
          otherwise. *)
  end
end

(** Percussion sounds.

    Percussion sounds can be created by using the [`Percussion]
    instrument values and the appropriate
    {{!Percussion.sound_pitch}sound pitch}. These notes are performed
    on channel 10 during MIDI rendering. *)
module Percussion : sig

  (** {1:percussion Sounds} *)

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
  (** The type for percussion sounds as per General MIDI
      {{:https://en.wikipedia.org/wiki/General_MIDI#Percussion}percussion}. *)

  (** {1:sound_pitch Sounds as pitches} *)

  val to_pitch : t -> Pitch.t
  (** [to_pitch snd] is [Pitch.of_abs (]{!to_abs_pitch}[ snd)]. *)

  val to_abs_pitch : t -> Pitch.abs
  (** [to_abs_pitch s] is the MIDI note used to represent percusion sound
      [s]. The result is in the range \[{!midi_min};{!midi_max}\]. *)

  val midi_min : Pitch.abs
  (** [midi_min] is [35] the lowest MIDI note used to represent
      percussion sounds in General MIDI. *)

  val midi_max : Pitch.abs
  (** [midi_max] is [81] the highest MIDI note used to represent percussion
      sounds in General MIDI. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal i0 i1] is [true] iff [i0] and [i1] are the same instrument. *)

  val compare : t -> t -> int
  (** [compare] is a total order on instruments compatible with [equal]. *)
end

(** Music. *)
module Music : sig

  (** {1:music Music} *)

  type dur = Q.t
  (** The type for durations in {{:https://en.wikipedia.org/wiki/Whole_note}
        whole notes} units. *)

  (** Durations. *)
  module Dur : sig

    (** {1:durations Durations} *)

    type t = dur
    (** See {!Music.type-dur}. *)

    (** {1:preds Predicates and comparisons} *)

    val equal : dur -> dur -> bool
    (** [equal] is {!Q.equal}. *)

    val compare : dur -> dur -> int
    (** [compare] is {!Q.compare}. *)

    (** {1:fmt Formatting} *)

    val pp : Format.formatter -> dur -> unit
    (** [pp] is {!Q.pp}. *)
  end

  (** Music modes. *)
  module Mode : sig
    type t =
      [ `Major | `Minor | `Ionian | `Dorian | `Phrygian | `Lydian
      | `Myxolydian | `Aeolian | `Locrian | `Custom of string ]
      (** The type for music modes. *)
  end

  (** Music phrasing.

      {b TODO.} Little of that is supported at the moment, see
      {!Phrase.attr}. *)
  module Phrase : sig

    type loudness =
    [ `Ppp | `Pp | `P | `Mp | `Mf | `F | `Ff | `Fff
    | `Volume of int (** in \[0;127\] *)  ]
    (** The type for loudness. Either relative markings or an absolute
        volume value.

        {b XXX.} For now the default performance interprets relative
        marking as absolute volume values as given in the diagram
        {{:https://en.wikipedia.org/wiki/Dynamics_(music)#Interpretation_by_notation_programs}here}. *)

    type dynamic =
    [ `Accent of Q.t
    | `Crescendo of Q.t
    | `Diminuendo of Q.t
    | `Loudness of loudness ]

    type tempo = [ `Ritardando of Q.t | `Accelerando of Q.t ]

    type articulation =
    [ `Staccato of Q.t (** Duration multiplier *)
    | `Legato of Q.t (** Duration mutiplier. *)
    | `Slurred of Q.t (** Duration mutiplier. *)
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
    [ `Dyn of dynamic (** Implemented: only [`Loudness] *)
    | `Tempo of tempo (** Not implemented. *)
    | `Art of articulation
       (** Implemented: only [`Staccato], [`Legato] and [`Slurred]. *)
    | `Orn of ornament (** Not implemented. *) ]

    type t = attr list
    (** The type for phrases. *)
  end

  (** Music control. *)
  module Ctrl : sig
    type t =
    [ `Tempo of Q.t (** Interpret with tempo multiplier. *)
    | `Transp of Pitch.rel (** Interpret transposed. *)
    | `Instrument of Instrument.t (** Interpet with instrument. *)
    | `Phrase of Phrase.t (** Interpret with phrasing. *)
    | `Player of string (** Not implemented *)
    | `Key_sig of Pitch.Class.t * Mode.t (** Not implemented *) ]
    (** The type for music control. *)
  end

  (** Music primitives. *)
  module Prim : sig

    (** {1:prims Primitives} *)

    type 'a t =
    [ `Note of dur * 'a (** Interpret note for given duration. *)
    | `Rest of dur (** Rest for given duration. *) ]
    (** The type for music primitives. Either a note of type ['a] or
        a rest (silence). *)

    val note : dur -> 'a -> 'a t
    (** [note d n] is [`Note (d, n)], interprets note [n] for duration [d]. *)

    val rest : dur -> 'a t
    (** [rest d] is [`Rest d], rests for duration [d]. *)

    (** {1:props Properties} *)

    val dur : 'a t -> dur
    (** [dur p] is [p]'s duration in whole notes units. *)

    (** {1:traversals Traversals} *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    (** [map f p] maps the note of [p] with [f]. *)

    val map_dur : (dur -> dur) -> 'a t -> 'a t
    (** [map_dur f p] maps duration of [p] with [f]. *)

    val fold : note:(dur -> 'a -> 'b) -> rest:(dur -> 'b) -> 'a t -> 'b
    (** [fold ~note ~rest p] folds over [p] with [note] and [rest]. *)
  end

  type 'a t =
  [ `Prim of 'a Prim.t (** Interpret primitive. *)
  | `Seq of 'a t * 'a t (** Interpret in sequence. *)
  | `Par of 'a t * 'a t (** Interpret in parallel. *)
  | `Ctrl of Ctrl.t * 'a t (** Interpret with control. *) ]
  (** The type for music with notes of type ['a]. *)

  val prim : 'a Prim.t -> 'a t
  (** [prim p] is [`Prim p], interprets primitive [p]. *)

  val seq : 'a t -> 'a t -> 'a t
  (** [seq m0 m1] is [`Seq (m0, m1)], interprets [m0] followed by [m1]. *)

  val par : 'a t -> 'a t -> 'a t
  (** [par m0 m1] is [`Par (m0, m1)], interprets [m0] in parallel with [m1]. *)

  val ctrl : Ctrl.t -> 'a t -> 'a t
  (** [ctrl c m] is [`Ctrl (c,  m)], applies control [c] to [m]. *)

  (** {1:properties Properties} *)

  val dur : 'a t -> dur
  (** [dur m] is [m]'s duration. *)

  (** {1:traverse Traversals} *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f m] maps [m]'s notes with [f]. *)

  val map_prims : ('a Prim.t -> 'b Prim.t) -> 'a t -> 'b t
  (** [map_prims f m] maps [m]'s primitives with [f]. *)

  val fold :
    prim:('a Prim.t -> 'b) -> seq:('b -> 'b -> 'b) -> par:('b -> 'b -> 'b) ->
    ctrl:(Ctrl.t -> 'b -> 'b) -> 'a t -> 'b
  (** [fold ~prim ~seq ~par ~ctrl m] folds over [m]'s constructors using
      [prim], [seq], [par] and [ctrl]. *)
end

(** Music definition module.

    This module provides short forms and derived combinators to
    define music concisely. It is meant to be used with [M.], [M.()],
    [M.[]] notations and, possibly, local [open]s. *)
module M : sig

  (** {1:basic Basic music} *)

  val note : Music.dur -> 'a -> 'a Music.t
  (** [note d p] is [`Prim (`Note (d, p))], interprets note [p] for duration
      [d]. See also {{!pitched_notes} pitched notes} constructors. *)

  val rest : Music.dur -> 'a Music.t
  (** [rest d] is [`Prim (`Rest d)], rests for duration [d]. *)

  val seq : 'a Music.t -> 'a Music.t -> 'a Music.t
  (** [seq m0 m1] is [`Seq (m0, m1)], interprets [m0] followed by [m1]. *)

  val par : 'a Music.t -> 'a Music.t -> 'a Music.t
  (** [par m0 m1] is [`Par (m0, m1)], interprets [m0] in parallel with [m1]. *)

  val ctrl : Music.Ctrl.t -> 'a Music.t -> 'a Music.t
  (** [ctrl c m] is [`Ctrl (c, m)]. *)

  val nil : 'a Music.t
  (** [nil] is [rest Q.zero], empty music. *)

  val line : 'a Music.t list -> 'a Music.t
  (** [line ms] interprets musics [ms] sequentially, in order. *)

  val chord : 'a Music.t list -> 'a Music.t
  (** [chord ms] iterprets musics [ms] in parallel. *)

  val times : int -> 'a Music.t -> 'a Music.t
  (** [times n m] repeats music [m] [n] times. Raises [Invalid_argument]
      if [n] is negative. *)

  val retro : 'a Music.t -> 'a Music.t
  (** [retro m] is the
      {{:https://en.wikipedia.org/wiki/Retrograde_(music)}retrograde}
      of [m]. *)

  val drop_zeros : 'a Music.t -> 'a Music.t
  (** [drop_zeros m] removes zero duration rests and notes from [m]. *)

  (** {1:break Breaking} *)

  val take : Music.dur -> 'a Music.t -> 'a Music.t
  (** [take d m] is music [m] for duration [d]. *)

  val drop : Music.dur -> 'a Music.t -> 'a Music.t
  (** [drop d m] is the music of [m] after [d]. *)

  (** {1:ornaments Ornaments} *)

  val grace : Q.t -> Pitch.rel -> Pitch.t Music.t -> Pitch.t Music.t
  (** [grace rd rp m] is the note [m] with a
      {{:https://en.wikipedia.org/wiki/Grace_note} grace note}
      added. Given [d = Music.dur m], this becomes two notes: the note
      [m] transposed by [rp] with duration [rd * d] followed by the note [m]
      with duration [(1 - r) * d].

      Raises [Invalid_argument] if [m] is not a note.

      {b XXX.} We could make it polymorphic by using {!transp}. *)

  (** {1:instruments Percussion and instruments} *)

  val perc : Percussion.t -> Music.dur -> Pitch.t Music.t
  (** [perc p d] is percusion sound [p] during [d]. *)

  val instrument : Instrument.t -> 'a Music.t -> 'a Music.t
  (** [instrument i m] is [`Ctrl (`Instrument i) m], interprets
      [m] with instrument [i].

      {b Note.} [instrument i' (instrument i m)] interprets
      [m] with [i]. Use {!change_instrument} if you want
      to change the instrument in [m]. *)

  val drop_instruments : 'a Music.t -> 'a Music.t
  (** [drop_instruments m] removes all instruments from [m]. *)

  val change_instrument : Instrument.t -> 'a Music.t -> 'a Music.t
  (** [change_instrument i m] is [instrument i (drop_instrument m)].  *)

  (** {1:ctrl Other controls} *)

  val transp : Pitch.rel -> 'a Music.t -> 'a Music.t
  (** [transp r m] is [ctrl (`Transpose n) m], transposes [m] by [r]
      relative semitones. Note that this does not alterate actual pitch
      values in [m] (the function does not know the note representation). *)

  val phrase : Music.Phrase.t -> 'a Music.t -> 'a Music.t
  (** [phrase atts m]  is [`Ctrl (`Phrase atts) m]. *)

  val key_sig : Pitch.Class.t -> Music.Mode.t -> 'a Music.t -> 'a Music.t
  (** [key_sig c mode m] is [`Ctrl (`Key_sig (c, mode)) m]. *)

  (** {1:time Timing} *)

  val offset : Music.dur -> 'a Music.t -> 'a Music.t
  (** [offset d m] is [rest d ^ m]. *)

  val tempo : Q.t -> 'a Music.t -> 'a Music.t
  (** [tempo r m] is [ctrl (`Tempo r) m], multiplies the tempo of [m]
      by [r]. If [r] is [Q.(2/1)] [m] is played twice as fast
      (duration are divided by [m]). Note that in contrast to
      {!mul_durs}, this does not change actual durations in [m]. *)

  val mul_durs : Q.t -> 'a Music.t -> 'a Music.t
  (** [mul_durs r m] multiplies durations in [m] by [r]. In contrast
      to {!tempo} this acts on the actual duration values in [m]. *)

  (** {2:durations Durations}

      Using initials of the
      {{:https://en.wikipedia.org/wiki/Note_value#List} American
      English standard}, except for double whole note where breve note
      is used ([bn]) due to a name clash with dotted whole note
      ([dwn]). *)

  val bn : Music.dur
  (** [bn] is [2/1], breve note (double whole note). *)

  val wn : Music.dur
  (** [wn] is [1/1], whole note. *)

  val hn : Music.dur
  (** [hn] is [1/2], half note. *)

  val qn : Music.dur
  (** [qn] is [1/4], quarter note. *)

  val en : Music.dur
  (** [en] is [1/8], eighth note. *)

  val sn : Music.dur
  (** [sn] is [1/16], sixteenth note. *)

  val tsn : Music.dur
  (** [tsn] is [1/32], thirty-second note. *)

  val sfn : Music.dur
  (** [sfn] is [1/64], sixty-fourth note. *)

  val dwn : Music.dur
  (** [dwn] is [3/2], dotted whole note. *)

  val dhn : Music.dur
  (** [dhn] is [3/4], dotted half note. *)

  val dqn : Music.dur
  (** [dqn] is [3/8], dotted quarted note. *)

  val den : Music.dur
  (** [den] is [3/16], dotted height note. *)

  val dsn : Music.dur
  (** [dsn] is [3/32], dotted sixteenth note. *)

  val dtsn : Music.dur
  (** [dtsn] is [3/64], dotted thirty-second note. *)

  val ddhn : Music.dur
  (** [ddhn] is [7/8], double dotted half note. *)

  val ddqn : Music.dur
  (** [ddqn] is [7/16], double dotted quarter note. *)

  val dden : Music.dur
  (** [dden] is [7/32], double dotted eight note. *)

  (** {2:rests Rests} *)

  (** The names are those of {{!durations}durations} suffixed by [r]. *)

  val bnr : 'a Music.t
  (** See {!bn}. *)

  val wnr : 'a Music.t
  (** See {!wn}. *)

  val hnr : 'a Music.t
  (** See {!hn}. *)

  val qnr : 'a Music.t
  (** See {!qn}. *)

  val enr : 'a Music.t
  (** See {!en}. *)

  val snr : 'a Music.t
  (** See {!sn}. *)

  val tsnr : 'a Music.t
  (** See {!tsn}. *)

  val sfnr : 'a Music.t
  (** See {!sfn}. *)

  val dwnr : 'a Music.t
  (** See {!dwn}. *)

  val dhnr : 'a Music.t
  (** See {!dhn}. *)

  val dqnr : 'a Music.t
  (** See {!dqn}. *)

  val denr : 'a Music.t
  (** See {!den}. *)

  val dsnr : 'a Music.t
  (** See {!dsn}. *)

  val dtsnr : 'a Music.t
  (** See {!dtsn}. *)

  val ddhnr : 'a Music.t
  (** See {!ddhn}. *)

  val ddqnr : 'a Music.t
  (** See {!ddqn}. *)

  val ddenr : 'a Music.t
  (** See {!dden}. *)

  (** {1:pitched_notes Pitched notes}

      The names are those of {!Pitch.Class.t} constructors
      lowercased. {!as'} is primed since [as] is an OCaml keyword. *)

  val cff : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [cff o d] is [note d (`Cff, o)] *)

  val cf : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [cf o d] is [note d (`Cf, o)] *)

  val c : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [c o d] is [note d (`C, o)] *)

  val dff : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [dff o d] is [note d (`Dff, o)] *)

  val cs : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [cs o d] is [note d (`Cs, o)] *)

  val df : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [df o d] is [note d (`Df, o)] *)

  val css : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [css o d] is [note d (`Css, o)] *)

  val d : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [d o d] is [note d (`D, o)] *)

  val eff : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [eff o d] is [note d (`Eff, o)] *)

  val ds : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [ds o d] is [note d (`Ds, o)] *)

  val ef : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [ef o d] is [note d (`Ef, o)] *)

  val fff : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [fff o d] is [note d (`Fff, o)] *)

  val dss : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [dss o d] is [note d (`Dss, o)] *)

  val e : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [e o d] is [note d (`E, o)] *)

  val ff : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [ff o d] is [note d (`Ff, o)] *)

  val es : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [es o d] is [note d (`Es, o)] *)

  val f : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [f o d] is [note d (`F, o)] *)

  val gff : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [gff o d] is [note d (`Gff, o)] *)

  val ess : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [ess o d] is [note d (`Ess, o)] *)

  val fs : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [fs o d] is [note d (`Fs, o)] *)

  val gf : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [gf o d] is [note d (`Gf, o)] *)

  val fss : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [fss o d] is [note d (`Fss, o)] *)

  val g : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [g o d] is [note d (`G, o)] *)

  val aff : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [aff o d] is [note d (`Aff, o)] *)

  val gs : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [gs o d] is [note d (`Gs, o)] *)

  val af : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [af o d] is [note d (`Af, o)] *)

  val gss : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [gss o d] is [note d (`Gss, o)] *)

  val a : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [a o d] is [note d (`A, o)] *)

  val bff : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [bff o d] is [note d (`Bff, o)] *)

  val as' : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [as' o d] is [note d (`As, o)] *)

  val bf : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [bf o d] is [note d (`Bf, o)] *)

  val ass : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [ass o d] is [note d (`Ass, o)] *)

  val b : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [b o d] is [note d (`B, o)] *)

  val bs : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [bs o d] is [note d (`Bs, o)] *)

  val bss : Pitch.octave -> Music.dur -> Pitch.t Music.t
  (** [bss o d] is [note d (`Bss, o)] *)

  (** {1:operators Operators}

      Both operators are right associative, [^] is tighter than [@|@].
      Open {!Mu.Syntax} if you want them in your scope. *)

  val ( ^ ) : 'a Music.t -> 'a Music.t -> 'a Music.t
  (** [m0 ^ m1] is {!Music.seq}[ m0 m1]. *)

  val ( @|@ ) : 'a Music.t -> 'a Music.t -> 'a Music.t
  (** [m0 @|@ m1] is {!Music.par}[ m0 m1]. *)
end

(** Music syntax support.

    Open this module to use it. *)
module Syntax : sig

  (** {1:operators Operators}

        Both operators are right associative, [^] is tighter than [@|@]. *)

  val ( ^ ) : 'a Music.t -> 'a Music.t -> 'a Music.t
  (** [m0 ^ m1] is {!Music.seq}[ m0 m1]. *)

  val ( @|@ ) : 'a Music.t -> 'a Music.t -> 'a Music.t
  (** [m0 @|@ m1] is {!Music.par}[ m0 m1]. *)
end

(** {1:notes Notes and performances} *)

(** Performable notes.

    A concrete representation for notes, used to derive
    {{!Mu.Performance.of_music}performances}. *)
module Pnote : sig

  type volume = int
  (** The type for volume. Using the MIDI convention, from [[0;127]]. *)

  type attr =
  [ `Volume of volume
  | `Fingering of int (** Not implemented *)
  | `Dynamics of int  (** Not implemented *)
  | `Params of float list (** Not implemented *) ]
  (** The type for note attributes. *)

  type t = Pitch.t * attr list
  (** The type for notes. *)

  (** {1:operations Operations} *)

  val transp : Pitch.rel -> t -> t
  (** [transp r note] transposes the pitch of [note] by [r] relative
      semitones. *)

  (** {1:converting Converting} *)

  val of_pitch : Pitch.t -> t
  (** [of_pitch p] is [(p, [])]. *)

  val of_pitch_volume : Pitch.t * volume -> t
  (** [of_pitch_volume (p,v)] is [(p, [`Volume v])]. *)

  val of_abs_pitch : Pitch.abs -> t
  (** [of_abs_pitch ap] is [(Pitch.of_abs ap, [])] *)

  val of_abs_pitch_volume : Pitch.abs * volume -> t
  (** [of_abs_pitch ap] is [(Pitch.of_abs ap, [`Volume v])] *)
end

(** Performances.

    Performances interpret music values as sequences of
    {{!Performance.Event}note events} on an absolute timeline. *)
module Performance : sig

  (** {1:time_and_dur Volume, time and duration} *)

  type volume = int
  (** The type for performance volume. Using the MIDI convention,
      from [[0;127]]. *)

  type time_s = Q.t
  (** The type for absolute performance times in seconds. The origin
      is [0]. *)

  type dur_s = Q.t
  (** The type for durations in seconds. *)

  val wn_dur_s : bpm:int -> beat_dur:Music.dur -> dur_s
  (** [wn_dur_s ~bpm ~beat_dur] is the duration, in seconds, of a
      whole note given [bpm] beats per minute and a beat duration of
      [beat_dur] whole notes.  For example [wn_dur_s ~bpm:120
      ~beat_dur:M.qn] is the duration, in seconds, of a whole note for
      120 beats per minute and one quarter note per beat. *)

  (** {1:note_events Note events} *)

  (** Note events. *)
  module Event : sig

    type t
    (** The type for music note events. *)

    val time_s : t -> time_s
    (** The time at which the event occurs. *)

    val instrument : t -> Instrument.t
    (** [instrument e] is the instrument performing the note event. *)

    val pitch : t -> Pitch.abs
    (** [pitch e] is the absolute pitch of the note event. *)

    val dur_s : t -> dur_s
    (** [dur_s e] is the duration of the note event. *)

    val volume : t -> volume
    (** [volume e] is the volume of the note event. *)

    val params : t -> float list
    (** [params e] are parameters of the note event. *)

    (** {1:preds Predicates and comparisons} *)

    val order_by_time : t -> t -> int
    (** [order_by_time e0 e1] is [Q.compare (time e0) (time e1)]. *)
  end

  (** {1:performances Performances} *)

  type t = Event.t list
  (** The type for performances. A list of note events ordered
      by time. *)

  val empty : t
  (** [empty] is the empty performance. *)

  val merge : t -> t -> t
  (** [merge p0 p1] merges performance [p0] and [p0]. *)

  val by_instrument : t -> t Instrument.Map.t
  (** [by_instrument p] maps each instrument of [p] to its own
      performance. *)

  (** {1:music Music intepretation} *)

  (** Musical context.

      The context is the context in which music is interpreted. *)
  module Ctx : sig

    type 'a player
    (** The type for players. {b TODO} *)

    type 'a t
    (** The type for musical contexts. *)

    val v :
      ?init:'a t -> ?time_s:time_s -> ?wn_dur_s:dur_s ->
      ?instrument:Instrument.t -> ?player:'a player -> ?transp:Pitch.rel ->
      ?volume:volume -> unit -> 'a t
    (** [v ~init ()] is a context with values from [init] (defaults to
        {!default}) except for those overriden explictely. *)

    val time_s : 'a t -> time_s
    (** [time_s c] is the absolute performance time of [c]. *)

    val wn_dur_s : 'a t -> dur_s
    (** [wn_dur_s c] is the duration of a whole note in [c]. *)

    val instrument : 'a t -> Instrument.t
    (** [instrument c] is the instrument performing in [c]. *)

    val player : 'a t -> 'a player
    (** [player c] is the player performing in [c]. *)

    val transp : 'a t -> Pitch.rel
    (** [transp c] is the pitch transposition in [c]. *)

    val volume : 'a t -> volume
    (** [volume c] is the performance volumne in [c]. *)

    val default : Pnote.t t
    (** [default] is a default context for intepreting {!Pnote.t} music
        in which:
        {ul
        {- [time_s] is {!Q.zero}.}
        {- [wn_dur_s] is [wn_dur_s ~bpm:120 ~beat:M.qn].}
        {- [instrument] is `[`Acoustic_grand_piano].}
        {- [player] is TODO}
        {- [transp] is [0].}
        {- [volume] is [127].}} *)
  end

  val of_music : ?ctx:Pnote.t Ctx.t -> Pnote.t Music.t -> t
  (** [of_music ~cxt m] is a performance given by intepreting music [m]
      starting in context [ctx]. The latter defaults to {!Ctx.default}
      in particular this means:
      {ul
      {- A tempo of 120 beats per minute with one beat for a quarter note.}
      {- A volume set to [127].}
      {- An instrument set to [`Acoustic_grand_piano]}} *)
end


(** {1:example Example}


{[
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
]}
*)

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
