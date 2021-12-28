open B0_kit.V000
open B00_std
open Result.Syntax

(* OCaml library names *)

let b0_b00_std = B0_ocaml.libname "b0.b00.std"
let mu = B0_ocaml.libname "mu"
let mu_player = B0_ocaml.libname "mu.player"

(* Libraries *)

let mu_lib =
  let srcs =
    Fpath.[ `File (v "src/mu.mli"); `File (v "src/mu.ml");
            `File (v "src/mu_midi.mli"); `File (v "src/mu_midi.ml") ]
  in
  let requires = [] in
  B0_ocaml.lib mu ~doc:"The mu library" ~srcs ~requires

let mu_player_lib =
  let srcs = Fpath.[ `File (v "src/mu_player.mli");
                     `File (v "src/mu_player.ml") ]
  in
  let requires = [b0_b00_std; mu] in
  B0_ocaml.lib mu_player ~doc:"The mu player library" ~srcs ~requires

(* Tests *)

let test_src f = `File Fpath.(v "test" // f)
let test_exe file ~doc =
  let file = Fpath.v file in
  let srcs = [test_src file] in
  let requires = [ b0_b00_std; mu; mu_player ] in
  B0_ocaml.exe (Fpath.basename ~no_ext:true file) ~doc ~srcs ~requires

let test_q = test_exe "test_q.ml" ~doc:"Mu.Q tests"
let test = test_exe "test.ml" ~doc:"Mu tests"
let twinkle = test_exe "twinkle.ml" ~doc:"Twinkle song"
let chick_corea = test_exe "chick_corea.ml" ~doc:"Chick Corea song"
let more_music = test_exe "more_music.ml" ~doc:"More music"
let prefixes = test_exe "prefixes.ml" ~doc:"Prefix song"
let song = test_exe "song.ml" ~doc:"Test song"
let scales = test_exe "scales.ml" ~doc:"Scales"
let shepard = test_exe "shepard.ml" ~doc:"Shepard tone"
let loudness = test_exe "loudness.ml" ~doc:"Loudness"
let loudness = test_exe "articulation.ml" ~doc:"Articulation"

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The mu programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/mu"
    |> add online_doc "https://erratique.ch/software/mu/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/mu.git"
    |> add issues "https://github.com/dbuenzli/mu/issues"
    |> add description_tags ["codec"; "midi"; "music"; "org:erratique"; ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.12.0"|};
        "b0", {|>= "0.0.3"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|}; ]
  in
  B0_pack.v "default" ~doc:"mu package" ~meta ~locked:true @@
  B0_unit.list ()
