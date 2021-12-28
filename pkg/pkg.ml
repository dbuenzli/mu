#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mu" @@ fun c ->
  Ok [ Pkg.mllib "src/mu.mllib";
       Pkg.mllib "src/mu_player.mllib" ~dst_dir:"player";
       Pkg.lib "src/mu_top_init.ml";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.test "test/test" ]
