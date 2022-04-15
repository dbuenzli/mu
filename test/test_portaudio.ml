(*---------------------------------------------------------------------------
   Copyright (c) 2022 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let pp_option pp_v ppf = function
| None -> Format.pp_print_string ppf "<none>" | Some v -> pp_v ppf v

let log fmt = Format.kfprintf (Fun.const ()) Format.std_formatter (fmt ^^ "@.")
let log_if_error ~use = function
| Ok v -> v
| Error e -> log "Error: %s" (Tportaudio.Error.message e); use

let test_version () = log "Testing portaudio %s" (Tportaudio.version ()); ()
let test_setup () =
  log "Testing initialize and terminate.";
  assert (Tportaudio.initialize () = Ok ());
  assert (Tportaudio.terminate () = Ok ());
  ()

let test_host_apis () =
  let report i a () =
    Tportaudio.Host_api_info.(log "  Found API %d: %d %s" i (type' a) (name a))
  in
  log "Testing host API functions";
  log_if_error ~use:() @@ Result.join @@ Tportaudio.bracket @@ fun () ->
  let* ac = Tportaudio.get_host_api_count () in
  let* da = Tportaudio.get_default_host_api () in
  log "  Host API count: %d default: %d" ac da;
  Tportaudio.fold_host_api_infos report ()

let test_devices () =
  let pp_dd = pp_option Format.pp_print_int in
  let report did d () =
    let open Tportaudio.Device_info in
    log "  Found device %d:@[<v> %s@,\
         max i/o channels: %d/%d@,\
         default i/o low latency: %gs/%gs@,\
         default i/o high latency: %gs/%gs@,\
         default sample-rate: %gHz@]"
      did (name d) (max_input_channels d) (max_output_channels d)
      (default_low_input_latency d) (default_low_output_latency d)
      (default_high_input_latency d) (default_high_output_latency d)
      (default_sample_rate_hz d)
  in
  log "Testing device functions";
  log_if_error ~use:() @@ Result.join @@ Tportaudio.bracket @@ fun () ->
  let* dc = Tportaudio.get_device_count () in
  let did = Tportaudio.get_default_input_device () in
  let dod = Tportaudio.get_default_output_device () in
  log "  Device count: %d default-input: %a default-output: %a"
    dc pp_dd did pp_dd dod;
  Tportaudio.fold_device_infos report ()

let test_write layout = (* as per paex_write_sine.c portaudio example. *)
  let gain = 0.1 in
  let sine_table ~size =
    let sin_tab i = gain *. sin ((float i /. (float size)) *. Float.pi *. 2.) in
    Array.init size sin_tab
  in
  let log_device_info info =
    log "  @[<v>Device: %s@,Channels: %d @@ %gHz"
      (Tportaudio.Device_info.name info)
      (Tportaudio.Device_info.max_output_channels info)
      (Tportaudio.Device_info.default_sample_rate_hz info)
  in
  let msg = match layout with
  | `Interleaved -> "interleaved" | `Planar -> "planar (may fail)"
  in
  log "Testing writing %s sine for 3s (right channel is higher)" msg;
  log_if_error ~use:() @@ Result.join @@ Tportaudio.bracket @@ fun () ->
  let device = Tportaudio.get_default_output_device () in
  let none = Tportaudio.Error.device_unavailable in
  let* device = Option.to_result ~none device in
  let* info = Tportaudio.get_device_info device in
  log_device_info info;
  let suggested_latency, sample_rate_hz =
    Tportaudio.Device_info.(default_low_output_latency info,
                            default_sample_rate_hz info)
  in
  let sample_format = Tportaudio.Float32 in
  let channel_count = 2 and frame_count = 1024 in
  let input = None in
  let output =
    Option.some @@ Tportaudio.stream_parameters
      ~device ~channel_count sample_format layout ~suggested_latency
  in
  let* () = Tportaudio.is_format_supported ~input ~output ~sample_rate_hz in
  let* s =
    let frames_per_buffer = Some frame_count in
    Tportaudio.open_stream ~input ~output ~sample_rate_hz ~frames_per_buffer ()
  in
  let finally () = ignore (Tportaudio.close_stream s) in
  Fun.protect ~finally @@ fun () ->
  let buf =
    Tportaudio.Buffer.create ~channel_count sample_format layout ~frame_count
  in
  let sine_size = 200 in
  let sine_table = sine_table ~size:sine_size in
  let* () = Tportaudio.start_stream s in
  let lp = ref 0 and rp = ref 0 and left_inc = ref 1 and right_inc = ref 3 in
  for i = 0 to 2 do
    let secs = 1 in
    let buf_count = ((float secs) *. sample_rate_hz /. (float frame_count)) in
    let buf_count = truncate buf_count in
    for c = 0 to buf_count - 1 do
      for i = 0 to frame_count - 1 do
        begin match layout with
        | `Interleaved ->
            let k = i * channel_count in
            let b = (Tportaudio.Buffer.data buf).(0) in
            Tportaudio.Buffer.set b k sine_table.(!lp);
            Tportaudio.Buffer.set b (k + 1) sine_table.(!rp);
        | `Planar ->
            let l = (Tportaudio.Buffer.data buf).(0) in
            let r = (Tportaudio.Buffer.data buf).(1) in
            Tportaudio.Buffer.set l i sine_table.(!lp);
            Tportaudio.Buffer.set r i sine_table.(!rp);
        end;
        lp := !lp + !left_inc;
        if !lp >= sine_size then lp := !lp - sine_size;
        rp := !rp + !right_inc;
        if !rp >= sine_size then rp := !rp - sine_size;
      done;
      log_if_error ~use:() (Tportaudio.write_stream s buf ~frame_count);
    done;
    incr left_inc; incr right_inc; (* Make pitch higher for next sec *)
  done;
  Tportaudio.stop_stream s

let main () =
  test_version ();
  test_setup ();
  test_host_apis ();
  test_devices ();
  test_write `Interleaved;
  test_write `Planar;
  Gc.full_major ();
  log "All tests succeeded!"

let () = if !Sys.interactive then () else main ()

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
