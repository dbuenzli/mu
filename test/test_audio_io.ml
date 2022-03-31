(*---------------------------------------------------------------------------
   Copyright (c) 2022 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

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
