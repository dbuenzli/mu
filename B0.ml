open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let mu = B0_ocaml.libname "mu"
let mu_player = B0_ocaml.libname "mu.player"
let mu_tportaudio = B0_ocaml.libname "mu.tportaudio"
let mu_tportmidi = B0_ocaml.libname "mu.tportmidi"

(* Libraries *)

let mu_lib =
  let srcs =
    Fpath.[ `File (v "src/mu.mli"); `File (v "src/mu.ml");
            `File (v "src/mu_midi.mli"); `File (v "src/mu_midi.ml") ]
  in
  let requires = [] in
  B0_ocaml.lib mu ~doc:"The mu library" ~srcs ~requires

let mu_tportaudio_lib =
  let stubs = `File (Fpath.v "src/tportaudio_stubs.c") in
  let srcs = Fpath.[stubs; `File (v "src/tportaudio.mli");
                    `File (v "src/tportaudio.ml") ]
  in
  let requires = [] in
  let c_requires = Cmd.(arg "-lportaudio") in
  B0_ocaml.lib mu_tportaudio ~doc:"portaudio binding library" ~srcs
    ~requires ~c_requires

let mu_tportmidi_lib =
  let stubs = `File (Fpath.v "src/tportmidi_stubs.c") in
  let srcs = Fpath.[stubs; `File (v "src/tportmidi.mli");
                    `File (v "src/tportmidi.ml") ]
  in
  let requires = [] in
  let c_requires = Cmd.(arg "-lportmidi") in
  B0_ocaml.lib mu_tportmidi ~doc:"portmidi binding library" ~srcs
    ~requires ~c_requires

let mu_player_lib =
  let srcs = Fpath.[ `File (v "src/mu_player.mli");
                     `File (v "src/mu_player.ml") ]
  in
  let requires = [b0_std; mu] in
  B0_ocaml.lib mu_player ~doc:"The mu player library" ~srcs ~requires

(* Tests *)

let test_src f = `File Fpath.(v "test" // f)

let test_exe ?(requires = []) file ~doc =
  let file = Fpath.v file in
  let srcs = [test_src file] in
  let requires = b0_std :: mu :: mu_player :: requires in
  B0_ocaml.exe (Fpath.basename ~strip_ext:true file) ~doc ~srcs ~requires

let test_q = test_exe "test_q.ml" ~doc:"Mu.Q tests"
let test = test_exe "test.ml" ~doc:"Mu tests"
let test_portaudio =
  let requires = [mu_tportaudio] in
  test_exe "test_portaudio.ml" ~doc:"Tportaudio tests" ~requires

let test_audio_io =
  let requires = [mu_tportaudio] in
  test_exe "test_audio_io.ml" ~doc:"Tportaudio IO pass through test" ~requires

let test_portmidi =
  let requires = [mu_tportmidi] in
  test_exe "test_portmidi.ml" ~doc:"Tportmidi tests" ~requires

let test_audio_io =
  let requires = [mu_tportmidi] in
  test_exe "test_midi_io.ml" ~doc:"Tportmidi IO test" ~requires

let twinkle = test_exe "twinkle.ml" ~doc:"Twinkle song"
let chick_corea = test_exe "chick_corea.ml" ~doc:"Chick Corea song"
let more_music = test_exe "more_music.ml" ~doc:"More music"
let prefixes = test_exe "prefixes.ml" ~doc:"Prefix song"
let song = test_exe "song.ml" ~doc:"Test song"
let scales = test_exe "scales.ml" ~doc:"Scales"
let shepard = test_exe "shepard.ml" ~doc:"Shepard tone"
let loudness = test_exe "loudness.ml" ~doc:"Loudness"
let articulation = test_exe "articulation.ml" ~doc:"Articulation"

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The mu programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/mu"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/mu/doc"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/mu.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/mu/issues"
    |> B0_meta.(add description_tags)
      ["codec"; "midi"; "music"; "org:erratique"; ]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-conf-portaudio" "%{conf-portaudio:installed}%"
          "--with-conf-portmidi" "%{conf-portmidi:installed}%"]]|}
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.12.0"|};
        "b0", {|>= "0.0.3"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "conf-portaudio", {|build|};
        "conf-portmidi", {|build|};
      ]
  in
  B0_pack.make "default" ~doc:"mu package" ~meta ~locked:true @@
  B0_unit.list ()
