(*---------------------------------------------------------------------------
   Copyright (c) 2022 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Thin bindings to the {{:https://github.com/PortMidi/portmidi}portmidi}
    library.

    Consult the
    {{:https://portmidi.github.io/portmidi_docs/}
    portmidi documentation} and an {{!example}example}. *)

(** {1:err Errors} *)

type error
(** The type for
    {{:https://portmidi.github.io/portmidi_docs/group__grp__basics.html#ga5fd46ccd2e320e17a840886731e8c6b9}
    errors}. *)

(** Errors. *)
module Error : sig

  type t = error
  (** See {!error}. *)

  val equal : error -> error -> bool
  (** [equal e0 e1] is [true] iff [e0] is equal to [e1]. *)

  val message : error -> string
  (** [message e] is an error message for [e].
      If [e] is {!host_error} this returns
      the {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#aad573f208b60577f21d2777a7c5054e0}host error text}. *)

  (** {1:cst Error constants} *)

  val host_error : t
  val invalid_device_id : t
  val insufficient_memory : t
  val buffer_too_small : t
  val buffer_overflow : t
  val bad_ptr : t
  val bad_data : t
  val internal_error : t
  val buffer_max_size : t
  val not_implemented : t
  val interface_not_supported : t
  val name_conflict : t
end

val error_string : ('a, error) result -> ('a, string) result
(** [error_string r] is [Result.map_error Error.message r]. *)

(** {1:setup Library setup} *)

val initialize : unit -> (unit, error) result
(** [initialize ()]
    {{:https://portmidi.github.io/portmidi_docs/group__grp__basics.html#ga5e9d0c116061f0cc25fb789fc393b417}initializes} the library. *)

val terminate : unit -> (unit, error) result
(** [terminate ()] terminates the library. This must be called
    before exiting a program that used {!initialize}. *)

val bracket : (unit -> 'a) -> ('a, error) result
(** [bracket f] runs [f ()] after an {!initialize} and calls
    {!terminate} after it finished, however it returned. *)

(** {1:devices Devices} *)

(** Device information. *)
module Device_info : sig
  type t
  (** The type for device information.

      Values of this type survive a {!terminate} but the refered
      host API index will no longer be valid. *)

  val interface : t -> string
  (** [interface d] is the underlying MIDI API. *)

  val name : t -> string
  (** [name d] is the device name. *)

  val input : t -> bool
  (** [input d] is [true] iff input is available. *)

  val output : t -> bool
  (** [output d] is [true] iff output is available. *)

  val is_virtual : t -> bool
  (** [is_virtual b] is [true] iff this is a virtual device. *)
end

type device_id = int
(** The type for device ids. Range from \[[0]; {!count_devices}[ () - 1]\]. *)

val count_devices : unit -> (int, error) result
(** [count_devices ()] is the device count. *)

val get_device_info : device_id -> (Device_info.t, error) result
(** [get_device_info did] is the information for device id [did]. *)

val fold_device_infos :
  (device_id -> Device_info.t -> 'a -> 'a) -> 'a -> ('a, error) result
(** [fold_device_infos d acc] folds over devices. Since devices may
    be deleted without re-enumeration this silently ignores
    {!Error.invalid_device_id} errors. *)

(** {1:virtual Virtual devices} *)

val create_virtual_input :
  name:string -> interface:string -> (unit, error) result
(** [create_virtual_input ~name ~string] creates a
    {{:https://portmidi.github.io/portmidi_docs/group__grp__device.html#gaf262cf560e105c9a44f7b30eb868951e}
    virtual MIDI input device}. *)

val create_virtual_output :
  name:string -> interface:string -> (unit, error) result
(** [create_virtual_output ~name ~string] creates a
    {{:https://portmidi.github.io/portmidi_docs/group__grp__device.html#gad72a83e522ff11431c2ab1fc87508e9e}
    virtual MIDI output device}. *)

val delete_virtual_device : device_id -> (unit, error) result
(** [delete_virtual_device did] delete the virtual device [did]. *)

(** {1:stream Streams}

    {b Warning.} You must not use streams after a {!terminate} this
    may lead to segfaults. *)

type stream
(** The type for {{:https://portmidi.github.io/portmidi_docs/group__grp__basics.html#gaf4949219ee1bb0afc857cb242d123914}
    MIDI streams}.

    {b Note.} After a stream is {{!close}closed} any function
    on the stream raises [Invalid_argument]. However at the moment
    using an unclosed stream value after a library {!terminate} will
    likely result in a segmentation fault. *)

val open_input : device_id -> event_buffer_size:int -> (stream, error) result
(** [open_input did ~buffer_size] {{:https://portmidi.github.io/portmidi_docs/group__grp__device.html#gabd50a31baaa494ad8b405f9ad54c966e}
    opens} a stream on input device [did] that buffers [event_buffer_size] MIDI
    events. *)

val open_output :
  device_id -> event_buffer_size:int -> latency:int -> (stream, error) result
(** [open_output did ~buffer_size ~latency] {{:https://portmidi.github.io/portmidi_docs/group__grp__device.html#ga134924cfa8badeecff3c5e1f22aee178}
    opens} a stream on output device [did] that buffers [event_buffer_size]
    MIDI events and adds [latency] to event time stamps. *)

val close : stream -> (unit, error) result
(** [close s] {{:https://portmidi.github.io/portmidi_docs/group__grp__events__filters.html#ga6b26be2fbcf092c9625ade5303edd0ac}
    closes} the stream [s]. *)

val abort : stream -> (unit, error) result
(** [abort s] {{:https://portmidi.github.io/portmidi_docs/group__grp__events__filters.html#ga8934ac3b02359b60177a06580c66a20e}
    aborts} the output stream [s]. *)

val synchronize : stream -> (unit, error) result
(** [synchronize s] {{:https://portmidi.github.io/portmidi_docs/group__grp__events__filters.html#gad3706ec801ec96010b82f950f89d2d15}
    resynchronizes} the stream [s]. *)

(** {2:rw Reading and writing} *)

(** Buffers of events *)
module Events : sig

  type msg = int32
  (** The type for MIDI messages. These are structured by [portmidi] as follows:
      {ul
      {- [(m land 0xFF)] is the MIDI status byte.}
      {- [(m lsr 8) land 0xFF] is first data byte.}
      {- [(m lsr 16) land 0xFF] is the second data byte.}}
      Consult {{:https://www.cs.cmu.edu/~music/cmsip/readings//Standard-MIDI-file-format-updated.pdf}this document} for the semantics of messages.

      {b TODO.} Should we try to get ints for easier OCaml manipulation ? *)

  type timestamp = int32
  (** The type for MIDI time stamps. *)

  type t
  (** The type for buffer of events. *)

  val create : int -> t
  (** [create count] is a buffer with [count] events. *)

  val count : t -> int
  (** [count evs] is the number of events in [evs]. *)

  val msg : t -> int -> msg
  (** [msg evs i] is the message of the [ith] event in [evs]. *)

  val timestamp : t -> int -> timestamp
  (** [timestamp evs i] is the timestamp of the [ith] event in [evs]. *)

  val set_msg : t -> int -> msg -> unit
  (** [set_msg evs i msg] sets the message of the [ith] event in [evs] to
      [msg]. *)

  val set_timestamp : t -> int -> timestamp -> unit
  (** [set_timetamp evs i t] sets the timestamp of the [ith] event in [evs]
      to [t]. *)
end

val poll : stream -> (bool, error) result
(** [poll s] {{:https://portmidi.github.io/portmidi_docs/group__grp__io.html#ga54198ca9dc1af9d82ec5f44f661faeea}
    polls} [s] for input data. *)

val read : stream -> Events.t -> event_count:int -> (int, error) result
(** [read s evs ~event_count] {{:https://portmidi.github.io/portmidi_docs/group__grp__io.html#ga3d59225bc890ede974f245ada3de6456}
    reads} at most [event_count] events in [evs] from [s] and returns the
    number of events effectively read. *)

val write : stream -> Events.t -> event_count:int -> (unit, error) result
(** [write s evs ~event_count] {{:https://portmidi.github.io/portmidi_docs/group__grp__io.html#ga82950117f003d28f9ca6536c00af985a}
    writes} [event_count] events from [evs] to [s]. *)

val write_short :
  stream -> when':Events.timestamp -> Events.msg -> (unit, error) result
(** [write_short s ~when' msg]
    {{:https://portmidi.github.io/portmidi_docs/group__grp__io.html#ga7a4100b808ac5892aae302f2a227d1ba}
    writes} message [msg] for [when'] to [s]. *)

val write_sysex :
  stream -> when':Events.timestamp -> string -> (unit, error) result
(** [write_sysex when' msg]
    {{:https://portmidi.github.io/portmidi_docs/group__grp__io.html#ga4bd5753bdfb53a1611bcdf5ba159fc87}
    writes} sysex message [msg] for [when'] to [s]. *)

(** {1:example Example}

    This example finds the first MIDI input device, create a virtual MIDI output
    called [mu.forward] on the same interface to and forwards the MIDI
    input on it.

{[
(* Forwards MIDI input to a virtual MIDI output.

   Compile with:

   ocamlfind ocamlopt \
   -package mu.tportmidi -linkpkg -o test_midi_io test_midi_io.ml

   ocamlfind ocamlc \
   -package mu.tportmidi -linkpkg -o test_midi_io test_midi_io.ml *)

let ( let* ) = Result.bind

let log fmt = Format.kfprintf (Fun.const ()) Format.std_formatter (fmt ^^ "@.")
let log_if_error ~use = function
| Ok v -> v | Error e -> log "Error: %s" (Tportmidi.Error.message e); use

let first_io_devices () =
  let first id info (i, o) =
    let i = match i with
    | None when Tportmidi.Device_info.input info -> Some (id, info)
    | Some _ | None as i ->  i
    in
    let o = match o with
    | None when Tportmidi.Device_info.output info -> Some (id, info)
    | Some _ | None as o -> o
    in
    (i, o)
  in
  Tportmidi.fold_device_infos first (None, None)

let virtual_output ~i = match i with
| None -> Ok None
| Some (_, i) ->
    let interface = Tportmidi.Device_info.interface i in
    let name = "mu.forward" in
    let* () = Tportmidi.create_virtual_output ~name ~interface in
    let get did i acc = (* Now look up its id. *)
      if Tportmidi.Device_info.name i = name then Some (did, i) else acc
    in
    Tportmidi.fold_device_infos get None

let log_device id d () =
  let open Tportmidi.Device_info in
  log "Found device %d: @[<v>%s (%s)@,in:%b out:%b virtual:%b@]"
    id (name d) (interface d) (input d) (output d) (is_virtual d)

let log_event evs k =
  let ts = Tportmidi.Events.timestamp evs k in
  let msg = Tportmidi.Events.msg evs k in
  let status = Int32.((to_int msg) land 0xFF) in
  let b0 = Int32.((to_int (shift_right msg 8)) land 0xFF) in
  let b1 = Int32.((to_int (shift_right msg 16)) land 0xFF) in
  Printf.printf "recv @ %ld: %x % 3d % 3d\n%!" ts status b0 b1

let forward stop ~i ~o = match i with
| None -> log "No MIDI input device found."; Ok ()
| Some (i, iinfo) ->
    let o, oinfo  = Option.get o in
    log_device i iinfo ();
    log_device o oinfo ();
    let c = 512 in
    let evs = Tportmidi.Events.create c in
    let* si = Tportmidi.open_input i ~event_buffer_size:c in
    let* so = Tportmidi.open_output o ~event_buffer_size:c ~latency:0 in
    let finally () = ignore (Tportmidi.close si); ignore (Tportmidi.close so) in
    Fun.protect ~finally @@ fun () ->
    let rec loop stop = match !stop with
    | true -> Ok ()
    | false ->
        let rc = log_if_error ~use:0 (Tportmidi.read si evs ~event_count:c) in
        if rc > 0 then begin
          log_if_error ~use:() (Tportmidi.write so evs ~event_count:rc);
          for k = 0 to rc - 1 do log_event evs k done
        end;
        loop stop
    in
    loop stop

let signal_stopper () =
  let stop = ref false in
  let stop_it = Sys.Signal_handle (fun _ -> stop := true) in
  Sys.set_signal Sys.sigint stop_it;
  Sys.set_signal Sys.sigabrt stop_it;
  stop

let main () =
  log_if_error ~use:1 @@ Result.join @@ Tportmidi.bracket @@ fun () ->
  let stop = signal_stopper () in
  let* i, _ = first_io_devices () in
  let* o = virtual_output ~i in
  Result.map (fun () -> 0) (forward stop ~i ~o)

let () = if !Sys.interactive then () else exit (main ())
]}
*)

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The mu programmers

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
