(*---------------------------------------------------------------------------
   Copyright (c) 2021 The mu programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open B0_std
open Result.Syntax
open Mu

let midi_file m =
  let p = Performance.of_music m in
  Mu_midi.File.(to_string (of_performance p))

let find_player () = match Os.Cmd.find (Cmd.tool "timidity") with
| Some cmd -> Ok cmd
| None ->
    match Os.Cmd.find Cmd.(tool "vlc" % "-I" % "dummy") with
    | Some cmd -> Ok cmd
    | None ->
        Error "Couldn't find a MIDI player.\nTry to install timidity or VLC"

let with_midi_file m f =
  Result.join @@ Os.File.with_tmp_oc ~name:"mu-%s.mid" @@ fun file oc ->
  output_string oc (midi_file m); close_out oc; f file

let play m =
  Log.if_error ~use:() @@
  let* player = find_player () in
  with_midi_file m @@ fun file ->
  Os.Cmd.run Cmd.(player %% path file)

let write_midi_file path m =
  Log.if_error ~use:() @@
  Os.File.write ~force:true ~make_path:true path (midi_file m)

let main m =
  let strf = Printf.sprintf in
  let exec = Filename.basename Sys.executable_name in
  let usage = strf "Usage: %s [-o FILE.mid]" exec in
  let file = ref "" in
  let args = [ "-o", Arg.Set_string file,  "Write music to given file."] in
  let pos s = raise (Arg.Bad (Fmt.str "Don't know what to do with %S" s)) in
  Arg.parse args pos usage;
  if !file = "" then play m else write_midi_file (Fpath.v !file) m

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
