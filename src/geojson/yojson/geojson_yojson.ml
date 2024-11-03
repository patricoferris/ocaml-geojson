module Yojson_parser = struct
  module J = Yojson.Safe

  type t = J.t

  let catch_err f v =
    try Ok (f v) with J.Util.Type_error (s, _) -> Error (`Msg s)

  let find_one s v =
    match catch_err (J.Util.member s) v with
    | Ok `Null -> None
    | Ok v -> Some v
    | Error (`Msg m) -> failwith m

  let rec find v = function
    | [] -> Some v
    | key :: keys -> (
        match find_one key v with Some v2 -> find v2 keys | None -> None)

  let to_string t = catch_err J.Util.to_string t
  let string s = `String s
  let to_float t = catch_err J.Util.to_float t
  let float f = `Float f
  let to_int t = catch_err J.Util.to_int t
  let int v = `Int v
  let to_list f t = catch_err (fun t -> J.Util.to_list t |> List.map f) t
  let list f t = `List (List.map f t)
  let to_array f t = Result.map Array.of_list @@ to_list f t
  let array f t = list f (Array.to_list t)
  let to_obj t = catch_err J.Util.to_assoc t
  let obj v = `Assoc v
  let null = `Null
  let is_null = function `Null -> true | _ -> false
end

include Geojson.Make (Yojson_parser)
