(*---------------------------------------------------------------------------
   Copyright (c) 2022 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let log fmt = Format.kfprintf (Fun.const ()) Format.std_formatter (fmt ^^ "@.")
let log_if_error ~use = function
| Ok v -> v
| Error e -> log "Error: %s" (Tportmidi.Error.message e); use

let test_setup () =
  log "Testing initialize and terminate.";
  assert (Tportmidi.initialize () = Ok ());
  assert (Tportmidi.terminate () = Ok ());
  ()

let log_device id d () =
  let open Tportmidi.Device_info in
  log "  Found device %d: @[<v>%s (%s)@,in:%b out:%b virtual:%b@]"
    id (name d) (interface d) (input d) (output d) (is_virtual d)

let test_devices () =
  log "Testing device functions";
  log_if_error ~use:() @@ Result.join @@ Tportmidi.bracket @@ fun () ->
  let* dc = Tportmidi.count_devices () in
  log "  Device count: %d" dc;
  Tportmidi.fold_device_infos log_device ()

let test_virtual_devices () =
  log "Testing virtual device functions";
  log_if_error ~use:() @@ Result.join @@ Tportmidi.bracket @@ fun () ->
  let interface =
    if Sys.win32 then "MMSystem" else
    if Sys.unix then "CoreMIDI" else "ALSA" (* FIXME *)
  in
  let* () = Tportmidi.create_virtual_input ~name:"mu.input" ~interface in
  let* () = Tportmidi.create_virtual_output ~name:"mu.output" ~interface in
  let report id d () = match Tportmidi.Device_info.is_virtual d with
  | false -> ()
  | true ->
      log_device id d ();
      log_if_error ~use:() (Tportmidi.delete_virtual_device id);
  in
  Tportmidi.fold_device_infos report ()

let main () =
  test_setup ();
  test_devices ();
  test_virtual_devices ();
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
