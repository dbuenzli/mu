(*---------------------------------------------------------------------------
   Copyright (c) 2022 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

(* Errors *)

module Error = struct
  type t = int
  let equal = Int.equal
  let to_int = Fun.id
  external _error_text : t -> string = "ocaml_tpm_get_error_text"
  external _host_error_text : unit -> string = "ocaml_tpm_get_host_error_text"

  let host_error = -10000
  let invalid_device_id = -999
  let insufficient_memory = -998
  let buffer_too_small = -997
  let buffer_overflow = -996
  let bad_ptr = -995
  let bad_data = -994
  let internal_error = -993
  let buffer_max_size = -992
  let not_implemented = -991
  let interface_not_supported = -990
  let name_conflict  = -989

  let message e =
    if e = host_error then _host_error_text () else _error_text e
end

type error = Error.t
let error_string r = Result.map_error Error.message r

let[@inline] ret r = if r < 0 then Error r else Ok ()
let[@inline] ret_int r = if r < 0 then Error r else Ok r
let[@inline] ret_bool r =
  if r < 0 then Error r else if r = 0 then Ok false else Ok true

(* Initialisation and termination  *)

external _initialize : unit -> int = "ocaml_tpm_initialize"
external _terminate : unit -> int = "ocaml_tpm_terminate"

let initialize () = ret (_initialize ())
let terminate () = ret (_terminate ())

let bracket f = match initialize () with
| Error _ as e -> e
| Ok () ->
    match f () with
    | v -> (match terminate () with Ok () -> Ok v | Error _ as e -> e)
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        ignore (terminate ());
        Printexc.raise_with_backtrace exn bt

(* Timestamps *)

type timestamp = int32

(* Devices *)

module Device_info = struct
  type t =
    { interface : string;
      name : string;
      input : bool;
      output : bool;
      is_virtual : bool }

  let interface d = d.interface
  let name d = d.name
  let input d = d.input
  let output d = d.output
  let is_virtual d = d.is_virtual
end

type device_id = int

external _count_devices : unit -> int = "ocaml_tpm_count_devices"
external get_device_info : device_id -> (Device_info.t, error) result =
  "ocaml_tpm_get_device_info"

let count_devices () = ret_int (_count_devices ())
let fold_device_infos f acc =
  let* count = count_devices () in
  let rec loop max i acc =
    if i > max then Ok acc else
    match get_device_info i with
    | Ok d -> loop max (i + 1) (f i d acc)
    | Error e when Error.(equal e invalid_device_id)  -> loop max (i + 1) acc
    | Error _ as e -> e
  in
  loop (count - 1) 0 acc

(* Virtual devices *)

external _create_virtual_input : string -> string -> int =
  "ocaml_tpm_create_virtual_input"

external _create_virtual_output : string -> string -> int =
  "ocaml_tpm_create_virtual_output"

external _delete_virtual_device : device_id -> int =
  "ocaml_tpm_delete_virtual_device"

let create_virtual_input ~name ~interface =
  ret (_create_virtual_input name interface)

let create_virtual_output ~name ~interface =
  ret (_create_virtual_output name interface)

let delete_virtual_device did = ret (_delete_virtual_device did)

(* Streams *)


(* Boxed pointer to a PmMidiStream value. N.B. we could devise a scheme
   to invalidate these pointers after a [terminate]. But… let's live
   dangerously for now. *)
type stream_ptr
type stream = { ptr : stream_ptr; mutable closed : bool; }

let[@inline] check s = if s.closed then invalid_arg "Stream is closed"

external _open_input : int -> int -> (stream_ptr, error) result =
  "ocaml_tpm_open_input"

external _open_output : int -> int -> int -> (stream_ptr, error) result =
  "ocaml_tpm_open_output"

external _close : stream_ptr -> int = "ocaml_tpm_close"
external _synchronize : stream_ptr -> int = "ocaml_tpm_synchronize"
external _abort : stream_ptr -> int = "ocaml_tpm_abort"
external _poll : stream_ptr -> int = "ocaml_tpm_poll"
external _read :
  stream_ptr -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t -> int -> int =
  "ocaml_tpm_read"

external _write :
  stream_ptr -> ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t -> int -> int =
  "ocaml_tpm_write"

external _write_short : stream_ptr -> int32 -> int32 -> int =
  "ocaml_tpm_write_short"

external _write_sysex : stream_ptr -> int32 -> string -> int =
  "ocaml_tpm_write_sysex"

let open_input did ~event_buffer_size =
  let* ptr = _open_input did event_buffer_size in
  Ok { ptr; closed = false }

let open_output did ~event_buffer_size ~latency =
  let* ptr = _open_output did event_buffer_size latency in
  Ok { ptr; closed = false }

let close s = check s; s.closed <- true; ret (_close s.ptr)
let abort s = check s; ret (_abort s.ptr)
let synchronize s = check s; ret (_synchronize s.ptr)

module Events = struct
  type msg = int32
  type timestamp = int32

  type t = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

  let create count =
    let ba = Bigarray.Array1.create Int32 C_layout (count * 2) in
    Bigarray.Array1.fill ba 0l; ba

  let count evs = Bigarray.Array1.dim evs / 2
  let[@inline] msg evs i = Bigarray.Array1.get evs (i * 2)
  let[@inline] timestamp evs i = Bigarray.Array1.get evs (i * 2 + 1)
  let[@inline] set_msg evs i m = Bigarray.Array1.set evs (i * 2) m
  let[@inline] set_timestamp evs i t = Bigarray.Array1.set evs (i * 2 + 1) t
end

let poll s = check s; ret_bool (_poll s.ptr)
let read s evs ~event_count = check s; ret_int (_read s.ptr evs event_count)
let write s evs ~event_count = check s; ret (_write s.ptr evs event_count)
let write_short s ~when' msg = check s; ret (_write_short s.ptr when' msg)
let write_sysex s ~when' msg = check s; ret (_write_sysex s.ptr when' msg)

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
