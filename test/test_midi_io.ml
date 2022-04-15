(*---------------------------------------------------------------------------
   Copyright (c) 2022 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

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
