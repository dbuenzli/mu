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
  external _error_text : t -> string = "ocaml_tpa_get_error_text"
  external _host_error_text : unit -> string = "ocaml_tpa_get_host_error_text"

  let not_initialized = -10000
  let unanticipated_host_error = -9999
  let invalid_channel_count = -9998
  let invalid_sample_rate = -9997
  let invalid_device = -9996
  let invalid_flag = -9995
  let sample_format_not_supported = -9994
  let bad_iodevice_combination = -9993
  let insufficient_memory = -9992
  let buffer_too_big = -9991
  let buffer_too_small = -9990
  let null_callback = -9989
  let bad_stream_ptr = -9988
  let timed_out = -9987
  let internal_error = -9986
  let device_unavailable = -9985
  let incompatible_host_api_specific_stream_Info = -9984
  let stream_is_stopped = -9983
  let stream_is_not_stopped = -9982
  let input_overflowed = -9981
  let output_underflowed = -9980
  let host_api_not_found = -9979
  let invalid_host_api = -9978
  let can_not_read_from_a_callback_stream = -9977
  let can_not_write_to_a_callback_stream = -9976
  let can_not_read_from_an_output_only_Stream = -9975
  let can_not_write_to_an_input_only_Stream = -9974
  let incompatible_stream_host_Api = -9973
  let bad_buffer_ptr = -9972

  let message e =
    if e = unanticipated_host_error then _host_error_text () else _error_text e
end

type error = Error.t
let error_string r = Result.map_error Error.message r

let[@inline] ret r = if r < 0 then Error r else Ok ()
let[@inline] ret_int r = if r < 0 then Error r else Ok r
let[@inline] ret_bool r =
  if r < 0 then Error r else if r = 0 then Ok false else Ok true

(* Initialisation and termination  *)

external _version : unit -> int * int * int * string * string =
  "ocaml_tpa_version"

external _initialize : unit -> int = "ocaml_tpa_initialize"
external _terminate : unit -> int = "ocaml_tpa_terminate"

let version () =
  let maj, min, patch, _, _ = _version () in
  Printf.sprintf "%d.%d.%d" maj min patch

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

(* Time *)

type time = float

external sleep_ms : int -> unit = "ocaml_tpa_sleep"

(* Host APIs *)

type device_index = int
let no_device = -1
let no_device_to_none = function -1 -> None | di -> Some di

module Host_api_info = struct
  module Type_id = struct
    type t = int
    let in_development = 0
    let direct_sound = 1
    let mme = 2
    let asio = 3
    let sound_manager = 4
    let core_audio = 5
    let oss = 7
    let alsa = 8
    let al = 9
    let beos = 10
    let wdmks = 11
    let jack = 12
    let wasapi = 13
    let audio_science_hpi = 14
    let audio_io = 15
  end

  type t =
    { type' : Type_id.t;
      name : string; devices : device_index list;
      default_input_device : device_index option;
      default_output_device : device_index option; }

  let type' a = a.type'
  let name a = a.name
  let devices a = a.devices
  let default_input_device a = a.default_input_device
  let default_output_device a = a.default_output_device
end

type host_api_index = int

external _get_host_api_count : unit -> int =
  "ocaml_tpa_get_host_api_count"

external _get_default_host_api : unit -> int =
  "ocaml_tpa_get_default_host_api"

external _get_host_api_info :
  host_api_index -> (int * string * int * int * int, error) result =
  "ocaml_tpa_get_host_api_info"

external _host_api_device_index_to_device_index :
  host_api_index -> int -> int =
  "ocaml_tpa_host_api_device_index_to_device_index"

let get_host_api_count () = ret_int (_get_host_api_count ())
let get_default_host_api () = ret_int (_get_default_host_api ())
let get_host_api_device_indexes idx device_count =
  let rec loop max i acc =
    if i > max then Ok (List.rev acc) else
    match _host_api_device_index_to_device_index idx i with
    | di when di >= 0 -> loop max (i + 1) (di :: acc)
    | e -> Error e
  in
  loop (device_count - 1) 0 []

let get_host_api_info idx =
  let* type', name, device_count, default_input_device, default_output_device =
    _get_host_api_info idx
  in
  let* devices = get_host_api_device_indexes idx device_count in
  let default_input_device = no_device_to_none default_input_device in
  let default_output_device = no_device_to_none default_output_device in
  Ok { Host_api_info.type'; name; devices; default_input_device;
       default_output_device }

let fold_host_api_infos f acc =
  let* count = get_host_api_count () in
  let rec loop max i acc =
    if i > max then Ok acc else
    match get_host_api_info i with
    | Ok h -> loop max (i + 1) (f i h acc)
    | Error _ as e -> e
  in
  loop (count - 1) 0 acc

(* Devices *)

module Device_info = struct
  type t =
    { name : string;
      host_api : host_api_index;
      max_input_channels : int;
      max_output_channels : int;
      default_low_input_latency : time;
      default_low_output_latency : time;
      default_high_input_latency : time;
      default_high_output_latency : time;
      default_sample_rate_hz : float; }

  let name d = d.name
  let host_api d = d.host_api
  let max_input_channels d = d.max_input_channels
  let max_output_channels d = d.max_output_channels
  let default_low_input_latency d = d.default_low_input_latency
  let default_low_output_latency d = d.default_low_output_latency
  let default_high_input_latency d = d.default_high_input_latency
  let default_high_output_latency d = d.default_high_output_latency
  let default_sample_rate_hz d = d.default_sample_rate_hz
end

external _get_device_count : unit -> int =
  "ocaml_tpa_get_device_count"

external _get_default_input_device : unit -> int =
  "ocaml_tpa_get_default_input_device"

external _get_default_output_device : unit -> int =
  "ocaml_tpa_get_default_output_device"

external get_device_info : device_index -> (Device_info.t, error) result =
  "ocaml_tpa_get_device_info"

let get_device_count () = ret_int (_get_device_count ())

let get_default_input_device () =
  no_device_to_none (_get_default_input_device ())

let get_default_output_device () =
  no_device_to_none (_get_default_output_device ())

let fold_device_infos f acc =
  let* count = get_device_count () in
  let rec loop max i acc =
    if i > max then Ok acc else
    match get_device_info i with
    | Ok d -> loop max (i + 1) (f i d acc)
    | Error _ as e -> e
  in
  loop (count - 1) 0 acc

(* Sample formats *)

type ('a, 'b) sample_format =
| Uint8 : (int, Stdlib.Bigarray.int8_unsigned_elt) sample_format
| Int8 : (int, Stdlib.Bigarray.int8_signed_elt) sample_format
| Int16 : (int, Stdlib.Bigarray.int16_signed_elt) sample_format
| Int24 : (int32, Stdlib.Bigarray.int32_elt) sample_format
| Int32 : (int32, Stdlib.Bigarray.int32_elt) sample_format
| Float32 : (float, Stdlib.Bigarray.float32_elt) sample_format

let zero_sample : type a b. (a, b) sample_format -> a = function
| Uint8 -> 128 | Int8 -> 0 | Int16 -> 0
| Int24 -> 0l | Int32 -> 0l | Float32 -> 0.

(* Audio buffers *)

module Buffer = struct

  (* Linear big arrays *)

  type ('a, 'b) array = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

  let ba_kind_of_sample_format :
    type a b. (a, b) sample_format -> (a, b) Bigarray.kind
  =
  function
  | Uint8 -> Int8_unsigned | Int8 -> Int8_signed | Int16 -> Int16_signed
  | Int24 -> Int32 | Int32 -> Int32 | Float32 -> Float32

  external ba_ptr : ('a, 'b) array -> nativeint = "ocaml_mu_ba_ptr"

  let[@inline] length ba = Bigarray.Array1.dim ba
  let[@inline] get ba i = Bigarray.Array1.get ba i
  let[@inline] set ba i v = Bigarray.Array1.set ba i v

  (* Buffers *)

  type layout = [ `Interleaved | `Planar ]

  type ('a, 'b) data =
  | Interleaved of ('a, 'b) array Stdlib.Array.t
  | Planar of { ptrs : (nativeint, Bigarray.nativeint_elt) array;
                channels : ('a, 'b) array Stdlib.Array.t }

  let data_interleaved ~channel_count sf ~frame_count =
    let size = channel_count * frame_count in
    let kind = ba_kind_of_sample_format sf in
    let a = Bigarray.Array1.create kind Bigarray.C_layout size in
    let () = Bigarray.Array1.fill a (zero_sample sf) in
    Interleaved (Array.init 1 (fun _ -> a))

  let data_planar ~channel_count sf ~frame_count =
    let sizes = [| channel_count; frame_count |] in
    let kind = ba_kind_of_sample_format sf in
    let a = Bigarray.Genarray.create kind Bigarray.C_layout sizes in
    let a1 = Bigarray.array1_of_genarray a in
    let () = Bigarray.Array1.fill a1 (zero_sample sf) in
    let channels =
      let channel i = Bigarray.Array1.sub a1 (i * frame_count) frame_count in
      Array.init channel_count channel
    in
    let ptrs =
      let cptr i = ba_ptr channels.(i) in
      Bigarray.(Array1.init Nativeint C_layout channel_count cptr)
    in
    Planar { ptrs; channels }

  type ('a, 'b) t =
    { channel_count : int;
      sample_format : ('a, 'b) sample_format;
      layout : layout;
      frame_count : int;
      data : ('a, 'b) data; }

  let create ~channel_count sample_format layout ~frame_count =
    let data = match layout with
    | `Interleaved -> data_interleaved ~channel_count sample_format ~frame_count
    | `Planar -> data_planar ~channel_count sample_format ~frame_count
    in
    { channel_count; sample_format; layout; frame_count; data }

  let channel_count b = b.channel_count
  let layout b = b.layout
  let sample_format b = b.sample_format
  let frame_count b = b.frame_count
  let data b = match b.data with
  | Interleaved d -> d
  | Planar { channels; _ } -> channels
end

(* Stream parameters *)

type ('a, 'b) stream_parameters =
  { device : device_index;
    channel_count : int;
    sample_format : int32;
    suggested_latency : time; }

let sample_format_to_pa_sample_format :
  type a b. (a, b) sample_format -> Buffer.layout -> int32 =
  fun sf layout ->
  let i = if layout = `Interleaved then 0l else 0x8000_0000l in
  let sf = match sf with
  | Uint8 -> 0x20l | Int8 -> 0x10l | Int16 -> 0x08l | Int24 -> 0x04l
  | Int32 -> 0x02l | Float32 -> 0x01l
  in
  Int32.logor i sf

let stream_parameters ~device ~channel_count sf layout ~suggested_latency
  =
  let sample_format = sample_format_to_pa_sample_format sf layout in
  { device; channel_count; sample_format; suggested_latency }

external _is_format_supported :
  ('i, 'is) stream_parameters option -> ('o, 'os) stream_parameters option ->
  float -> int =
  "ocaml_tpa_is_format_supported"

let is_format_supported ~input ~output ~sample_rate_hz =
  match _is_format_supported input output sample_rate_hz with
  | c when c = 0 -> Ok () | e -> Error e

(* Streams *)

module Stream_flags = struct
  type t = int
  let none = 0
  let clip_off = 0x1
  let dither_off = 0x2
  let ( + ) f0 f1 = f0 lor f1
end

(* Boxed pointer to a PaStream value. N.B. we could devise a scheme
   to invalidate these pointers after a [terminate]. But… let's live
   dangerously for now. *)
type ('i, 'is, 'o, 'os) stream_ptr

type ('i, 'is, 'o, 'os) stream =
  { ptr : ('i, 'is, 'o, 'os) stream_ptr;
    mutable closed : bool; }

external _open_stream :
  ('i, 'is) stream_parameters option -> ('o, 'os) stream_parameters option ->
  float -> int -> int -> (('i, 'is, 'o, 'os) stream_ptr, error) result =
  "ocaml_tpa_open_stream"

external _open_default_stream :
  int -> int -> int32 -> float -> int ->
  (('i, 'is, 'o, 'os) stream_ptr, error) result =
  "ocaml_tpa_open_default_stream"

external _get_stream_info :
  ('i, 'is, 'o, 'os) stream_ptr -> (time * time * float, error) result =
  "ocaml_tpa_get_stream_info"

external _start_stream : ('i, 'is, 'o, 'os) stream_ptr -> int =
  "ocaml_tpa_start_stream"

external _stop_stream : ('i, 'is, 'o, 'os) stream_ptr -> int =
  "ocaml_tpa_stop_stream"

external _abort_stream : ('i, 'is, 'o, 'os) stream_ptr -> int =
  "ocaml_tpa_abort_stream"

external _close_stream : ('i, 'is, 'o, 'os) stream_ptr -> int =
  "ocaml_tpa_close_stream"

external _is_stream_active : ('i, 'is, 'o, 'os) stream_ptr -> int =
  "ocaml_tpa_is_stream_active"

external _is_stream_stopped : ('i, 'is, 'o, 'os) stream_ptr -> int =
  "ocaml_tpa_is_stream_stopped"

external _get_stream_time : ('i, 'is, 'o, 'os) stream_ptr -> time =
  "ocaml_tpa_get_stream_time"

external _get_stream_read_available : ('i, 'is, 'o, 'os) stream_ptr -> int =
  "ocaml_tpa_get_stream_read_available"

external _get_stream_write_available : ('i, 'is, 'o, 'os) stream_ptr -> int =
  "ocaml_tpa_get_stream_write_available"

external _write_stream :
  ('i, 'is, 'o, 'os) stream_ptr -> ('a, 'b) Buffer.array -> int -> int =
  "ocaml_tpa_write_stream"

external _read_stream :
  ('i, 'is, 'o, 'os) stream_ptr -> ('a, 'b) Buffer.array -> int -> int =
  "ocaml_tpa_read_stream"

let frames_per_buffer_to_int = function None -> 0 | Some f -> f

let open_stream
    ?(stream_flags = Stream_flags.none) ~input ~output ~sample_rate_hz
    ~frames_per_buffer ()
  =
  let fpb = frames_per_buffer_to_int frames_per_buffer in
  let* ptr = _open_stream input output sample_rate_hz fpb stream_flags in
  Ok { ptr; closed = false }

let open_default_stream
    ~input_channel_count:ic ~output_channel_count:oc sf layout
    ~sample_rate_hz:sr ~frames_per_buffer
  =
  let sf = sample_format_to_pa_sample_format sf layout in
  let fpb = frames_per_buffer_to_int frames_per_buffer in
  let* ptr = _open_default_stream ic oc sf sr fpb in
  Ok { ptr; closed = false }

let[@inline] check s = if s.closed then invalid_arg "Stream is closed"

let get_stream_info s = check s; _get_stream_info s.ptr
let start_stream s = check s; ret (_start_stream s.ptr)
let stop_stream s = check s; ret (_stop_stream s.ptr)
let abort_stream s = check s; ret (_abort_stream s.ptr)
let close_stream s = check s; s.closed <- true; ret (_close_stream s.ptr)

let is_stream_active s = check s; ret_bool (_is_stream_active s.ptr)
let is_stream_stopped s = check s; ret_bool (_is_stream_stopped s.ptr)

let get_stream_time s = check s; _get_stream_time s.ptr

let get_stream_read_available s =
  check s; ret_int (_get_stream_read_available s.ptr)

let get_stream_write_available s =
  check s; ret_int (_get_stream_write_available s.ptr)

let write_stream s b ~frame_count:c = match b.Buffer.data with
| Buffer.Interleaved ba -> check s; ret (_write_stream s.ptr ba.(0) c)
| Buffer.Planar { ptrs; _ } -> check s; ret (_write_stream s.ptr ptrs c)

let read_stream s b ~frame_count:c = match b.Buffer.data with
| Buffer.Interleaved ba -> check s; ret (_read_stream s.ptr ba.(0) c)
| Buffer.Planar { ptrs; _ } -> check s; ret (_read_stream s.ptr ptrs c)

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
