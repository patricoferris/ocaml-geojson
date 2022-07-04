let print_geometry g =
  print_endline @@ Ezjsone.value_to_string (Geojsone.G.to_json g)

let print_property prop = print_endline @@ Ezjsone.value_to_string prop

let src_of_flow flow =
  let buff = Cstruct.create 2048 in
  fun () ->
    try
      let got = Eio.Flow.(read flow buff) in
      let t = Some (Cstruct.to_bytes buff, 0, got) in
      t
    with End_of_file -> None

let with_src cwd f func =
  Eio.Dir.with_open_in cwd f @@ fun ic -> func @@ src_of_flow ic

let () =
  Eio_main.run @@ fun env ->
  let or_fail = function
    | Ok () -> ()
    | Error e ->
        Geojsone.Err.pp Format.err_formatter e;
        failwith "Internal err"
  in
  let cwd = Eio.Stdenv.cwd env in
  or_fail
    ( with_src cwd "./input/simple.geojson" @@ fun src ->
      Geojsone.iter_geometry print_geometry src );
  or_fail
    ( with_src cwd "./input/simple.geojson" @@ fun src ->
      Geojsone.iter_props print_property src )
