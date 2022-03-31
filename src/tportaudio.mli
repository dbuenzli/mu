(*---------------------------------------------------------------------------
   Copyright (c) 2022 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Thin bindings to the {{:http://portaudio.com/}portaudio} library.

    Consult the
    {{:http://files.portaudio.com/docs/v19-doxydocs/index.html}
    portaudio documentation} and an {{!example}example}. *)

(** {1:err Errors} *)

type error
(** The type for errors. *)

(** Errors. *)
module Error : sig

  type t = error
  (** See {!error}. *)

  val equal : error -> error -> bool
  (** [equal e0 e1] is [true] iff [e0] is equal to [e1]. *)

  val message : error -> string
  (** [message e] is an error message for [e].
      If [e] is {!unanticipated_host_error} this returns
      the {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#aad573f208b60577f21d2777a7c5054e0}host error text}.
*)

  (** {1:cst Error constants} *)

  val not_initialized : error
  val unanticipated_host_error : error
  val invalid_channel_count : error
  val invalid_sample_rate : error
  val invalid_device : error
  val invalid_flag : error
  val sample_format_not_supported : error
  val bad_iodevice_combination : error
  val insufficient_memory : error
  val buffer_too_big : error
  val buffer_too_small : error
  val null_callback : error
  val bad_stream_ptr : error
  val timed_out : error
  val internal_error : error
  val device_unavailable : error
  val incompatible_host_api_specific_stream_Info : error
  val stream_is_stopped : error
  val stream_is_not_stopped : error
  val input_overflowed : error
  val output_underflowed : error
  val host_api_not_found : error
  val invalid_host_api : error
  val can_not_read_from_a_callback_stream : error
  val can_not_write_to_a_callback_stream : error
  val can_not_read_from_an_output_only_Stream : error
  val can_not_write_to_an_input_only_Stream : error
  val incompatible_stream_host_Api : error
  val bad_buffer_ptr : error
end

val error_string : ('a, error) result -> ('a, string) result
(** [error_string r] is [Result.map_error Error.message r]. *)

(** {1:setup Library setup} *)

val version : unit -> string
(** [version ()] is the [xx.yy.zz] version string of the library. *)

val initialize : unit -> (unit, error) result
(** [initialize ()]
    {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#abed859482d156622d9332dff9b2d89da}initializes} the library. *)

val terminate : unit -> (unit, error) result
(** [terminate ()]
    {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a0db317604e916e8bd6098e60e6237221}terminates} the library. This must be called
    before exiting a program that used {!initialize}. *)

val bracket : (unit -> 'a) -> ('a, error) result
(** [bracket f] runs [f ()] after an {!initialize} and calls
    {!terminate} after it finished, however it returned. *)

(** {1:time Time} *)

type time = float
(** The type for representing monotonic time in seconds. *)

val sleep_ms : int -> unit
(** [sleep_ms ms] sleeps for [ms] milliseconds. *)

(** {1:hostapi Host APIs} *)

type device_index = int
(** The type for device indexes. *)

(** Host API info. *)
module Host_api_info : sig

  (** Host API type identifiers. *)
  module Type_id : sig
    type t = int
    (** The type for {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a8eaebe3d39c5ea45598da8f86dc2e5ae}host API type identifiers}. *)

    val in_development : t
    val direct_sound : t
    val mme : t
    val asio : t
    val sound_manager : t
    val core_audio : t
    val oss : t
    val alsa : t
    val al : t
    val beos : t
    val wdmks : t
    val jack : t
    val wasapi : t
    val audio_science_hpi : t
    val audio_io : t
  end

  type t
  (** The type for host API information.

      Values of this type survive a {!terminate} but the refered
      device indexes will no longe be valid. *)

  val type' : t -> Type_id.t
  (** [type' a] is the type identifier of [a]. *)

  val name : t -> string
  (** [name a] is the name of [a]. *)

  val devices : t -> device_index list
  (** [devices a] are the indexes of the devices of [a]. *)

  val default_input_device : t -> device_index option
  (** [default_input_device a] is the device index of the default
      input device of [a]. *)

  val default_output_device : t -> device_index option
  (** [default_input_device a] is the device index of the default
      input device of [a]. *)
end

type host_api_index = int
(** The type for host API indexes ranges from
    \[[0];{!get_host_api_count}[ ()]\]. *)

val get_host_api_count : unit -> (int, error) result
(** [get_host_api_count ()] is the
    {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a19dbdb7c8702e3f4bfc0cdb99dac3dd9}
    number} of available host APIs. *)

val get_default_host_api : unit -> (host_api_index, error) result
(** [default_host_api ()] is the index of the
    {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#ae55c77f9b7e3f8eb301a6f1c0e2347ac}
    default host API}. *)

val get_host_api_info : host_api_index -> (Host_api_info.t, error) result
(** [get_host_api_info ai] is the host API info for index [ai]. *)

val fold_host_api_infos :
  (Host_api_info.t -> 'a -> 'a) -> 'a -> ('a, error) result
(** [fold_host_api_infos f acc] folds over the host APIs. *)

(** {1:devices Devices} *)

(** Device information. *)
module Device_info : sig
  type t
  (** The type for host API information.

      Values of this type survive a {!terminate} but the refered
      host API index will no longer be valid. *)

  val name : t -> string
  (** [name d] is the name of [d]. *)

  val host_api : t -> host_api_index
  (** [host_api d] is the host API of [d]. *)

  val max_input_channels : t -> int
  (** [max_input_channels d] is the maximum number of input channels. *)

  val max_output_channels : t -> int
  (** [max_output_channels d] is the maximum number of output channels. *)

  val default_low_input_latency : t -> time
  (** [default_low_input_latency d] is the default latency value for
      interactive performance. *)

  val default_low_output_latency : t -> time
  (** [default_low_input_latency d] is the default latency value for
      interactive performance. *)

  val default_high_input_latency : t -> time
  (** [default_high_input_latency d] is the default latency value for robust
      non-interactive application (e.g. playing sound files). *)

  val default_high_output_latency : t -> time
  (** [default_high_input_latency d] is the default latency value for robust
      non-interactive application (e.g. playing sound files). *)

  val default_sample_rate_hz : t -> float
  (** [default_sample_rate_hz d] is the default sample rate in hz. *)
end

val get_device_count : unit -> (int, error) result
(** [get_device_count ()] is the
    {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#acfe4d3c5ec1a343f459981bfa2057f8d}number}
    of available devices. *)

val get_default_input_device : unit -> device_index option
(** [get_default_input_device ()] is the index of the
    {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#abf9f2f82da95553d5adb929af670f74b}
    default input device}. *)

val get_default_output_device : unit -> device_index option
(** [get_default_output_device ()] is the index of the
    {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#adc955dfab007624000695c48d4f876dc}
    default output device}. *)

val get_device_info : device_index -> (Device_info.t, error) result
(** [get_device_info di] is the device information fo index [di]. *)

val fold_device_infos : (Device_info.t -> 'a -> 'a) -> 'a -> ('a, error) result
(** [fold_device_infos f acc] folds over devices. *)

(** {1:sample_formats Sample formats} *)

type ('a, 'b) sample_format =
| Uint8 : (int, Stdlib.Bigarray.int8_unsigned_elt) sample_format
| Int8 : (int, Stdlib.Bigarray.int8_signed_elt) sample_format
| Int16 : (int, Stdlib.Bigarray.int16_signed_elt) sample_format
| Int24 : (int32, Stdlib.Bigarray.int32_elt) sample_format
| Int32 : (int32, Stdlib.Bigarray.int32_elt) sample_format
| Float32 : (float, Stdlib.Bigarray.float32_elt) sample_format (** *)
(** The type for
    {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a4582d93c2c2e60e12be3d74c5fe00b96}
    sample formats}. *)

val zero_sample : ('a, 'b) sample_format -> 'a
(** [zero_sample sf] is the silence for sample format [sf]. *)

(** {1:audio_buffers Audio buffers} *)

(** Audio buffers. *)
module Buffer : sig

  (** {1:larray Linear bigarrays} *)

  type ('a, 'b) array = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
  (** The type for linear bigarrays. *)

  val length : ('a, 'b) array -> int
  (** [length] is {!Bigarray.Array1.dim}. *)

  val get : ('a, 'b) array -> int -> 'a
  (** [get] is {!Bigarray.Array1.get} *)

  val set : ('a, 'b) array -> int -> 'a -> unit
  (** [set] is {!Bigarray.Array1.set} *)

  (** {1:buffers Buffers} *)

  type layout = [ `Interleaved | `Planar ]
  (** The type for specifying how frames are laid out in buffers.
      See {!data}. *)

  type ('a, 'b) t
  (** The type for buffers of audio frames. *)

  val create :
    channel_count:int -> ('a, 'b) sample_format -> layout ->
    frame_count:int -> ('a, 'b) t
  (** [create ~channel_count sf layout ~frame_count] is a buffer of
      [frame_count] frames for [channel_count] channels of sample
      format [sf] laid out according to [layout] (see {!data}) and
      initially filled with {!zero_sample}. *)

  val channel_count : ('a, 'b) t -> int
  (** [channel_count b] is the number of channels in [b]. *)

  val sample_format : ('a, 'b) t -> ('a, 'b) sample_format
  (** [sample_format b] is the sample format of [b]. *)

  val layout : ('a, 'b) t -> layout
  (** [layout b] is the buffer layout of [b]. *)

  val frame_count : ('a, 'b) t -> int
  (** [frame_count b] is the number of frames in [b]. *)

  val data : ('a, 'b) t -> ('a, 'b) array Stdlib.Array.t
  (** [data b] is [d] the data of buffer [b]. If the layout of [b] is
      {ul
      {- [`Interleaved] the resulting array has a single element.
         The sample of channel [c] at frame [f] is given at
         [get d.(0) (f * channel_count + c)].}
      {- [`Planar], the resulting array one element per channel.
         The sample of channel [c] at frame [f] is given at
         [get d.(c) f].}} *)
end

(** {1:stream_parameters Stream parameters} *)

type ('a, 'b) stream_parameters
(** The type for
    {{:http://files.portaudio.com/docs/v19-doxydocs/structPaStreamParameters.html}
    streams parameters}. The type parameters witness the sample format. *)

val stream_parameters :
  device:device_index -> channel_count:int -> ('a, 'b) sample_format ->
  Buffer.layout -> suggested_latency:time -> ('a, 'b) stream_parameters
(** [stream_parameters ~device ~channel_count sample_format bf
    ~suggested_latency] are stream parameters with the given attributes. *)

val is_format_supported :
  input:('i, 'is) stream_parameters option ->
  output:('o, 'os) stream_parameters option ->
  sample_rate_hz:float -> (unit, error) result
(** [is_format_supported ~input ~output ~sample_rate_hz] is [Ok ()] if the
    given format is supported. *)

(** {1:stream Streams}

    {b Warning.} You must not use streams after a {!terminate} this
    may lead to segfaults. *)

(** Stream flags. *)
module Stream_flags : sig
  type t
  (** The type for
      {{:http://www.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#add037e93aec78fa8d67f7a59ed500707}
      stream flags} *)

  val none : t
  (** [none] has no flags set. *)

  val clip_off : t
  (** [clip_off] disables default clipping out of range samples. *)

  val dither_off : t
  (** [dither_off] disables default dithering. *)

  val ( + ) : t -> t -> t
  (** [f0 + f1] has the flags of [f0] and [f1]. *)
end

type ('i, 'is, 'o, 'os) stream
(** The type for streams. The type parameters respectively track the sample
    format of input and output channels.

    {b Note.} After a stream is {{!close_stream}closed} any function
    on the stream raises [Invalid_argument]. However at the moment
    using an unclosed stream value after a library {!terminate} will
    likely result in a segmentation fault. *)

val open_stream :
  ?stream_flags:Stream_flags.t ->
  input:('i, 'is) stream_parameters option ->
  output:('o, 'os) stream_parameters option ->
  sample_rate_hz:float -> frames_per_buffer:int option -> unit ->
  (('i, 'is, 'o, 'os) stream, error) result
(** [open_stream ~input ~output ~sample_rate_hz ~frames_per_buffer]
    {{:http://www.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a443ad16338191af364e3be988014cbbe}
    open a stream} a with given parameters. *)

val open_default_stream :
  input_channel_count:int -> output_channel_count:int ->
  ('a, 'b) sample_format -> Buffer.layout -> sample_rate_hz:float ->
  frames_per_buffer:int option -> (('a, 'b, 'a, 'b) stream, error) result
(** [open_default_stream ~input_channel_count ~output_channel_count
    bf ~sample_rate_hz]
    {{:http://www.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a0a12735ac191200f696a43b87667b714}
    opens a stream} on the default device with
    given parameters. *)

val close_stream : ('a, 'b, 'c, 'd) stream -> (unit, error) result
(** [close_stream s] {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a92f56f88cbd14da0e8e03077e835d104}
    closes} stream [s]
    (discards any pending buffers). Once [s] is closed any call on
    [s] raises [Invalid_argument]. *)

val get_stream_info :
  ('a, 'b, 'c, 'd) stream -> (time * time * float, error) result
(** [get_stream_info s] is the
    triple [input_latency, output_latency, sample_rate_hz] for [s]. *)

(** {2:start_stop Starting and stopping} *)

val start_stream : ('a, 'b, 'c, 'd) stream -> (unit, error) result
(** [start_stream s] {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a7432aadd26c40452da12fa99fc1a047b}
    starts} audio processing in [s]. *)

val stop_stream : ('a, 'b, 'c, 'd) stream -> (unit, error) result
(** [stop_stream s] {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#af18dd60220251286c337631a855e38a0}
    stops} audio processing in [s]. Blocks
    until pending buffers have been processed before it returns. See also
    {!abort_stream}. *)

val abort_stream : ('a, 'b, 'c, 'd) stream -> (unit, error) result
(** [stop_stream s] {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a138e57abde4e833c457b64895f638a25}
    stops} audio processing in [s]. Discards
    pending bufffers. See also {!stop_stream}. *)

val is_stream_active : ('a, 'b, 'c, 'd) stream -> (bool, error) result
(** [is_stream_active s] determines if [s] is {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a1f8709c4971932643681a6f374c4bb5a}
    active} (playing or recording audio). *)

val is_stream_stopped : ('a, 'b, 'c, 'd) stream -> (bool, error) result
(** [is_stream_stopped s] determines if [s] is {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a52d778c985ae9d566de7e13529cc771f}
    stopped}
    ({!stop_stream} or {!abort_stream} was called). *)

(** {2:rw Reading and writing} *)

val get_stream_time : ('a, 'b, 'c, 'd) stream -> time
(** [get_stream_time s] is the {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a2b3fb60e6949f37f7f134105ff425749}
    current time} of [s]. *)

val get_stream_read_available : ('a, 'b, 'c, 'd) stream -> (int, error) result
(** [get_stream_read_available s] is the
    {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#ad04c33f045fa58d7b705b56b1fd3e816}
    number of frames that can be read} from [s] without blocking. *)

val get_stream_write_available : ('a, 'b, 'c, 'd) stream -> (int, error) result
(** [get_stream_write_available s] is the
    {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a25595acf48733ec32045aa189c3caa61}
    number of frames that can be written} to [s] without blocking. *)

val write_stream :
  ('i, 'is, 'o, 'os) stream -> ('o, 'os) Buffer.t -> frame_count:int ->
  (unit, error) result
(** [write_stream s fs ~count] {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a075a6efb503a728213bdae24347ed27d}
    writes} [frame_count] frames from [fs] to stream [s], this may block. *)

val read_stream :
  ('i, 'is, 'o, 'os) stream -> ('i, 'is) Buffer.t -> frame_count:int ->
  (unit, error) result
(** [read_stream s fs ~count] {{:http://files.portaudio.com/docs/v19-doxydocs/portaudio_8h.html#a075a6efb503a728213bdae24347ed27d}
    reads} [frame_count] frames from [s] into [fs]. *)


(** {1:example Example}

    This example passes the default audio input to the default audio
    output.

{[
(* Passes default audio input to default audio output.

   Compile with:

   ocamlfind ocamlopt \
   -package mu.tportaudio -linkpkg -o test_audio_io test_audio_io.ml

   ocamlfind ocamlc \
   -package mu.tportaudio -linkpkg -o test_audio_io test_audio_io.ml *)


let ( let* ) = Result.bind

let log fmt = Format.kfprintf (Fun.const ()) Format.std_formatter (fmt ^^ "@.")
let log_if_error ~use = function
| Ok v -> v | Error e -> log "Error: %s" (Tportaudio.Error.message e); use

let log_stream_info iinfo oinfo s =
  let ms_lat l = truncate (l *. 1000.) in
  let iname, oname = Tportaudio.Device_info.(name iinfo, name oinfo) in
  let* ilat, olat, srate = Tportaudio.get_stream_info s in
  log "%s (lat:%dms) -- %gHz --> %s (lat:%dms)"
    iname (ms_lat ilat) srate oname (ms_lat olat);
  Ok ()

let io_stream sf layout ~sample_rate_hz ~frame_count ~i ~o =
  let* iinfo = Tportaudio.get_device_info i in
  let* oinfo = Tportaudio.get_device_info o in
  let channel_count =
    let ic = Tportaudio.Device_info.max_input_channels iinfo in
    let oc = Tportaudio.Device_info.max_output_channels oinfo in
    Int.min ic oc
  in
  let input =
    let ilat = Tportaudio.Device_info.default_low_input_latency iinfo in
    Option.some @@ Tportaudio.stream_parameters
      ~device:i ~channel_count sf layout ~suggested_latency:ilat
  in
  let output =
    let olat = Tportaudio.Device_info.default_low_output_latency oinfo in
    Option.some @@ Tportaudio.stream_parameters
      ~device:o ~channel_count sf layout ~suggested_latency:olat
  in
  let frames_per_buffer = Some frame_count in
  let* s =
    Tportaudio.open_stream ~input ~output ~sample_rate_hz ~frames_per_buffer ()
  in
  Ok (iinfo, oinfo, channel_count, s)

let run_io_pass stop s buf =
  let frame_count = Tportaudio.Buffer.frame_count buf in
  let* () = Tportaudio.start_stream s in
  let rec loop stop = match !stop with
  | true -> Tportaudio.stop_stream s
  | false ->
      log_if_error ~use:() (Tportaudio.write_stream s buf ~frame_count);
      log_if_error ~use:() (Tportaudio.read_stream s buf ~frame_count);
      loop stop
  in
  loop stop

let io_pass stop ~i ~o =
  let sample_rate_hz = 44100. and frame_count = 256 in
  let sf = Tportaudio.Float32 and layout = `Interleaved in
  let* ii, oi, c, s = io_stream sf layout ~sample_rate_hz ~frame_count ~i ~o in
  let finally () = ignore (Tportaudio.close_stream) in
  Fun.protect ~finally @@ fun () ->
  let* () = log_stream_info ii oi s in
  let buf = Tportaudio.Buffer.create ~channel_count:c sf layout ~frame_count in
  run_io_pass stop s buf

let default_io_devices () =
  let none = Tportaudio.Error.device_unavailable in
  let* i = Option.to_result ~none @@ Tportaudio.get_default_input_device () in
  let* o = Option.to_result ~none @@ Tportaudio.get_default_output_device () in
  Ok (i, o)

let signal_stopper () =
  let stop = ref false in
  let stop_it = Sys.Signal_handle (fun _ -> stop := true) in
  Sys.set_signal Sys.sigint stop_it;
  Sys.set_signal Sys.sigabrt stop_it;
  stop

let main () =
  log_if_error ~use:1 @@ Result.join @@ Tportaudio.bracket @@ fun () ->
  let stop = signal_stopper () in
  let* i, o = default_io_devices () in
  Result.map (fun () -> 0) (io_pass stop ~i ~o)

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
