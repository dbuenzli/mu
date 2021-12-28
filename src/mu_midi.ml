(*---------------------------------------------------------------------------
   Copyright (c) 2021 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* MIDI messages and events *)

type tick = int
type channel = int
type key = int
type velocity = int
type controller = int
type pressure = int
type program = int
type pitch_wheel = int
type tempo = int

module Msg = struct
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

(* MIDI files *)

module File = struct

  (* Cf. https://www.cs.cmu.edu/~music/cmsip/readings/\
         Standard-MIDI-file-format-updated.pdf *)

  type type' = Single_track | Multi_track | Multi_pattern
  type time_div =
  | Ticks_per_quarter_note of int
  | Ticks_per_second of int * int

  type track = event list
  type t = { type' : type'; time_div : time_div; tracks : track list }
  let v ~type' ~time_div tracks = { type'; time_div; tracks }

  let type' f = f.type'
  let time_div f = f.time_div
  let tracks f = f.tracks

  (* Encoding *)

  let type_to_uint16 = function
  | Single_track -> 0 | Multi_track -> 1 | Multi_pattern -> 2

  let time_div_to_uint16 = function
  | Ticks_per_quarter_note tpb -> (tpb land 0x7FFF)
  | Ticks_per_second (i1, i2) -> ((i1 lor 0x80) lsl 8) lor (i2 land 0xF)

  let enc_bytes b bytes = Buffer.add_string b bytes
  let enc_uint8 b i = Buffer.add_uint8 b i
  let enc_uint16_be b i = Buffer.add_uint16_be b i
  let enc_uint32_be b i = Buffer.add_int32_be b i
  let enc_varlen_be b i =
    let b0 = i lsr 21 in
    let b1 = i lsr 14 in
    let b2 = i lsr 07 in
    let b3 = i land 0x7F in
    if b0 <> 0 then
      (enc_uint8 b (b0 lor 0x80); enc_uint8 b (b1 lor 0x80);
       enc_uint8 b (b2 lor 0x80); enc_uint8 b b3)
    else if b1 <> 0 then
      (enc_uint8 b (b1 lor 0x80); enc_uint8 b (b2 lor 0x80); enc_uint8 b b3)
    else if b2 <> 0 then
      (enc_uint8 b (b2 lor 0x80); enc_uint8 b b3)
    else enc_uint8 b b3

  let enc_str b s = enc_varlen_be b (String.length s); enc_bytes b s
  let enc_meta_msg b code = enc_uint8 b 0xFF; enc_uint8 b code
  let enc_channel_msg b code channel =
    enc_uint8 b ((code lsl 4) lor (channel land 0xF))

  let enc_message b = function
  | Msg.Note_off { channel; key; velocity } ->
      enc_channel_msg b 0x08 channel; enc_uint8 b key; enc_uint8 b velocity
  | Note_on { channel; key; velocity } ->
      enc_channel_msg b 0x09 channel; enc_uint8 b key; enc_uint8 b velocity
  | Key_pressure { channel; key; pressure } ->
      enc_channel_msg b 0x0A channel; enc_uint8 b key; enc_uint8 b pressure
  | Control_change { channel; controller; value } ->
      enc_channel_msg b 0x0B channel; enc_uint8 b controller; enc_uint8 b value
  | Program_change { channel; program } ->
      enc_channel_msg b 0x0C channel; enc_uint8 b program
  | Channel_pressure { channel; pressure } ->
      enc_channel_msg b 0x0D channel; enc_uint8 b pressure
  | Pitch_wheel { channel; pitch_wheel = pb} ->
      enc_channel_msg b 0x0E channel;
      enc_uint8 b (pb land 0x7F);  enc_uint8 b ((pb lsr 7) land 0x7F)
  | Sequence_number seq ->
      enc_meta_msg b 0x00; enc_varlen_be b 2; enc_uint16_be b seq
  | Text s ->
      enc_meta_msg b 0x01; enc_str b s
  | Copyright s ->
      enc_meta_msg b 0x02; enc_str b s
  | Track_name s ->
      enc_meta_msg b 0x03; enc_str b s
  | Instrument_name s ->
      enc_meta_msg b 0x04; enc_str b s
  | Lyrics s ->
      enc_meta_msg b 0x05; enc_str b s
  | Marker s ->
      enc_meta_msg b 0x06; enc_str b s
  | Cue_point s ->
      enc_meta_msg b 0x07; enc_str b s
  | Program_name s ->
      enc_meta_msg b 0x08; enc_str b s
  | Device_name s ->
      enc_meta_msg b 0x09; enc_str b s
  | Channel_prefix c ->
      enc_meta_msg b 0x20; enc_varlen_be b 1; enc_uint8 b c
  | Track_end ->
      enc_meta_msg b 0x2F; enc_varlen_be b 0
  | Tempo_change c ->
      enc_meta_msg b 0x51; enc_varlen_be b 3;
      enc_uint8 b (c lsr 16); enc_uint8 b (c lsr 8); enc_uint8 b c;
  | Smtpe_offset (b0, b1, b2, b3, b4) ->
      enc_meta_msg b 0x54; enc_varlen_be b 5;
      enc_uint8 b b0; enc_uint8 b b1; enc_uint8 b b2; enc_uint8 b b3;
      enc_uint8 b b4;
  | Time_signature (b0, b1, b2, b3) ->
      enc_meta_msg b 0x58; enc_varlen_be b 4;
      enc_uint8 b b0; enc_uint8 b b1; enc_uint8 b b2; enc_uint8 b b3;
  | Key_signature (b0, b1) ->
      enc_meta_msg b 0x59; enc_varlen_be b 2;
      enc_uint8 b b0; enc_uint8 b b1;
  | Reserved (c, s) ->
      enc_meta_msg b c; enc_str b s
  | Sysex (i, s) ->
      enc_uint8 b i; enc_str b s

  let enc_event b (dt, m) = enc_varlen_be b dt; enc_message b m

  let track_events_bytes b t =
    Buffer.reset b; List.iter (enc_event b) t; Buffer.contents b

  let enc_track b track_event_bytes =
    Buffer.add_string b "MTrk";
    enc_uint32_be b (Int32.of_int (String.length track_event_bytes));
    enc_bytes b track_event_bytes

  let to_string m =
    let b = Buffer.create (500 * 1024) in
    let tracks = List.map (track_events_bytes b) m.tracks in
    Buffer.reset b;
    enc_bytes b "MThd";
    enc_uint32_be b 0x6l;
    enc_uint16_be b (type_to_uint16 m.type');
    enc_uint16_be b (List.length m.tracks);
    enc_uint16_be b (time_div_to_uint16 m.time_div);
    List.iter (enc_track b) tracks;
    Buffer.contents b

  (* Performance convertion *)

  open Mu

  module Tline = struct
    (* Time line sorted by largest time first (for the final fold) *)

    module T = struct type t = int let compare x y = Int.compare y x end
    include Map.Make (T)

    let add_to_list k v m = match find_opt k m with
    | None -> add k [v] m | Some l -> add k (v :: l) m
  end

  let bpm_120 = 500_000 (* 500_000 useconds/quarter note *)
  let ticks_per_quarter_note = 96
  let ticks_per_s = Q.int (2 * ticks_per_quarter_note) (* because 120 bpm *)
  let s_to_ticks secs = Q.(to_int (ticks_per_s * secs))

  let rel_time evs = (* relativize the time of [evs] and add track end msg *)
    let rec loop at = function
    | [] -> [0, Msg.Track_end]
    | (t, msg) :: evs -> let rt = t - at in (rt, msg) :: loop (at + rt) evs
    in
    loop 0 evs

  let add_event channel e tline =
    let d = Performance.Event.dur_s e in
    if Q.(d = zero) then tline else
    let t = Performance.Event.time_s e and key = Performance.Event.pitch e
    and velocity = Performance.Event.volume e in
    let t_note_on = s_to_ticks t and t_note_off = s_to_ticks Q.(t + d) in
    let note_on = Msg.Note_on {channel; key; velocity} in
    let note_off = Msg.Note_off {channel; key; velocity} in
    tline
    |> Tline.add_to_list t_note_on (t_note_on, note_on)
    |> Tline.add_to_list t_note_off (t_note_off, note_off)

  let make_track (channel, program, performance) =
    let tchange = (0, Msg.Tempo_change bpm_120) in
    let pchange = (0, Msg.Program_change { channel; program }) in
    let rec loop tline = function
    | [] -> Tline.fold (fun t es acc -> List.rev_append es acc) tline []
    | e :: es -> loop (add_event channel e tline) es
    in
    (tchange :: pchange :: rel_time (loop Tline.empty performance))

  let track_allocation instruments =
    let add_track i performance (next, tracks) =
      if next > 15 then failwith "too many instruments" else
      let next, channel, program = match i with
      | `Percussion -> next, 9 (* Channel 10 *), 0 (* Any should do *)
      | _ ->
          (if next + 1 = 9 then next + 2 else next + 1), next,
          Instrument.midi_program i
      in
      next, (channel, program, performance) :: tracks
    in
    snd (Instrument.Map.fold add_track instruments (0, []))

  let of_performance p =
    try
      let by_instrument = Performance.by_instrument p in
      let ts = track_allocation by_instrument in
      let type' = if List.length ts = 1 then Single_track else Multi_track in
      let time_div = Ticks_per_quarter_note ticks_per_quarter_note in
      let tracks = List.map make_track ts in
      { type'; time_div; tracks }
    with (* FIXME devise an error type and return a result *)
    | Failure e -> failwith e
    | Division_by_zero -> failwith "division by zero"
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
