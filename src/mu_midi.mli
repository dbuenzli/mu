(*---------------------------------------------------------------------------
   Copyright (c) 2021 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** MIDI rendering for {!Mu}.

    For now the interesting function is {!File.of_performance}. *)

(** {1:msgs Messages and events} *)

type channel = int
(** The type for channels. Ranges in \[0;15\]. *)

type tick = int
(** The type for ticks. Ranges in \[0;2{^28}-1\]. *)

type key = int
(** The type for keys (notes). Ranges in \[0;127\]. *)

type velocity = int
(** The type for velocity. Ranges in \[0;127\]. *)

type pressure = int
(** The type for pressure. Ranges in \[0;127\]. *)

type controller = int
(** The type for controller numbers. *)

type program = int
(** The type for programs. Ranges in \[0;127\]. *)

type pitch_wheel = int
(** The type for pitch wheel. Ranges in \[0;2{^14}-1\]. *)

type tempo = int
(** The type for MIDI tempo in usecs per beat. Ranges in \[0;2{^14}-1\]. *)

(** MIDI messages. *)
module Msg : sig
  type t =
  | Note_off of { channel : channel; key : key; velocity : velocity }
  | Note_on of { channel : channel; key : key; velocity : velocity }
  | Key_pressure of { channel : channel; key : key; pressure : pressure }
  | Control_change of
      { channel : channel; controller : controller; value : int }
  | Program_change of { channel : channel; program : program }
  | Channel_pressure of { channel : channel; pressure : pressure }
  | Pitch_wheel of { channel : channel; pitch_wheel : pitch_wheel }
  | Sequence_number of int
  | Text of string
  | Copyright of string
  | Track_name of string
  | Instrument_name of string
  | Lyrics of string
  | Marker of string
  | Cue_point of string
  | Channel_prefix of channel
  | Program_name of string
  | Device_name of string
  | Track_end
  | Tempo_change of tempo
  | Smtpe_offset of int * int * int * int * int
  | Time_signature of int * int * int * int
  | Key_signature of int * int
  | Reserved of int * string
  | Sysex of int * string
end

type event = tick * Msg.t
(** The type for MIDI events. In MIDI files the tick should be
    relative to the previous tick. *)

(** {1:files MIDI files} *)

(** MIDI files ([.mid]).

    MIDI files encoder. *)
module File : sig

  (** {1:files Files} *)

  type type' = Single_track | Multi_track | Multi_pattern (** *)
  (** The type for MIDI file types. *)

  type time_div =
  | Ticks_per_quarter_note of int (** \[0;2{^15}-1\] *)
  | Ticks_per_second of int * int (** *)
  (** The type for specifying the unit for ticks in the file. *)

  type track = event list
  (** A track is a list of MIDI events. The tick of an event is expressed
      relative to the previous tick. Tracks need to end with
      {!Msg.Track_end} event ({b XXX} The codec could handle that). *)

  type t
  (** The type for MIDI files. *)

  val v : type':type' -> time_div:time_div -> track list -> t
  (** [v ~type ~time_div ~tracks] is a MIDI file with given parameters. *)

  val type' : t -> type'
  (** [type' f] is [f]'s type. *)

  val time_div : t -> time_div
  (** [time_div f] is [f]'s time division. *)

  val tracks : t -> track list
  (** [tracks f] is the list of tracks of [f]. *)

  (** {1:enc Encoding} *)

  val to_string : t -> string
  (** [to_string m] is [m] as a MIDI file. *)

  (** {1:convert Converting} *)

  val of_performance : Mu.Performance.t -> t
  (** [of_performance p] is a MIDI file for peformance [p].

      {b FIXME.}

      {ul
      {- Do a proper error type and a [result], for now
      this raises [Failure _].}
      {- For now [p] is limited to use of 15 instruments (one is
      reserved for percussion). Each instrument gets assigned a different
      channel and lives in its own track. A few alternative strategies
      could be used here.}} *)
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
