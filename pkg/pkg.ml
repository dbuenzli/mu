#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let portaudio = Conf.with_pkg "conf-portaudio"
let portmidi = Conf.with_pkg "conf-portmidi"

let () =
  Pkg.describe "mu" @@ fun c ->
  let portaudio = Conf.value c portaudio in
  let portmidi = Conf.value c portmidi in
  Ok [ Pkg.mllib "src/mu.mllib";
       Pkg.lib "src/mu_top_init.ml";
       Pkg.mllib "src/mu_player.mllib" ~dst_dir:"player";
       Pkg.mllib ~cond:portaudio "src/tportaudio.mllib" ~dst_dir:"tportaudio";
       Pkg.clib ~cond:portaudio  "src/libtportaudio_stubs.clib"
         ~lib_dst_dir:"tportaudio";
       Pkg.mllib ~cond:portaudio "src/tportmidi.mllib" ~dst_dir:"tportmidi";
       Pkg.clib ~cond:portaudio  "src/libtportmidi_stubs.clib"
         ~lib_dst_dir:"tportmidi";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.test "test/test";
       Pkg.test ~cond:portaudio "test/test_portaudio";
       Pkg.test ~cond:portaudio "test/test_audio_io";
       Pkg.test ~cond:portmidi "test/test_portmidi";
       Pkg.test ~cond:portmidi "test/test_midi_io";

     ]
