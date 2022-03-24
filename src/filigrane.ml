(* open Cohttp *) 
open Cohttp_lwt_unix

let (let*) = Lwt.bind
let (and*) = Lwt.both

module Json = Yojson.Safe

type fetch_result = {
  candidate: Json.t; 
  reference: Json.t
}

type diff =
  | ObjectDiff of (string * diff) list
  | ArrayDiff of diff list
  | ValueDiff of Json.t * Json.t
  | MissingArrayElem of int
  | MissingObjectKey of string

let rec json_of_diff (diff:diff) : Json.t =
  match diff with
  | ObjectDiff l -> `Assoc(l |> List.map(fun (k,v) -> (k, json_of_diff v)))
  | ArrayDiff l -> `List(l |> List.map json_of_diff)
  | ValueDiff (candidate, reference) -> 
    `Assoc([
      ("kind", `String("value_diff"));
      ("candidate", candidate);
      ("reference", reference)
    ])
  | MissingArrayElem index ->
    `Assoc([
      ("kind", `String("missing_array_elem"));
      ("index", `Int(index))
    ])
  | MissingObjectKey key -> 
    `Assoc([
      ("kind", `String("missing_object_key"));
      ("key", `String(key))
    ])

type diff_result = {path: string; diff: diff option}

let json_of_diff_result result: Json.t =
  let diff_list = 
    result.diff 
      |> Option.map json_of_diff 
      |> Option.to_list
  in
  `Assoc([
    ("path", `String(result.path)); 
    ("diff", `List(diff_list))
  ])

let rec diff_json (candidate: Json.t) (reference:Json.t) =
    match (candidate, reference) with
    | (`Assoc(c), `Assoc(r)) -> diff_json_object c r
    | (`List(c), `List(r))   -> diff_json_array c r
    | (c, r)                 -> diff_json_value c r

and diff_json_value candidate reference =
    if Json.equal candidate reference then 
      None 
    else 
      Some(ValueDiff(candidate,reference))


and diff_json_array candidate reference =
  let unpack i x = (i,x) in
  let step (i,c) =
    match reference |> List.find_opt (Json.equal c) with 
    | Some e -> diff_json c e
    | None -> Some(MissingArrayElem i)
  in
  let diff_list = candidate 
    |> List.mapi unpack 
    |> List.filter_map step 
  in
  if List.length diff_list > 0 then
    Some(ArrayDiff diff_list)
  else
    None

and diff_json_object candidate reference =
  let find c (k,_) = String.equal c k in
  let step (k,v) =
    match reference |> List.find_opt (find k) with
    | Some (_, vv) -> diff_json v vv |> Option.map(fun (x) -> (k,x))
    | None -> Some((k,MissingObjectKey k))
  in
  let diff_list = candidate 
    |> List.filter_map step 
  in
  if List.length diff_list > 0 then
    Some(ObjectDiff diff_list)
  else
    None

let fetch_json uri =
  let* (_, body) = Client.get (Uri.of_string uri) in
  let* body_str = body |> Cohttp_lwt.Body.to_string in 
  body_str |> Json.from_string |> Lwt.return

let fetch_both_json candidate reference path =
  let* candidate_json = fetch_json (String.concat "" [candidate; path]) 
  and* reference_json = fetch_json (String.concat "" [reference; path]) in
  Lwt.return {candidate = candidate_json; reference = reference_json}  

let compute_diff_result candidate reference path =
  let* fetched = fetch_both_json candidate reference path in
  let diff = diff_json fetched.candidate fetched.reference in
  let json = json_of_diff_result {path; diff} in
  json |> Json.to_string |> print_endline;
  Lwt.return_unit

let main =
  let candidate = ref "" in
  let reference = ref "" in
  let paths = ref [] in

  let args = [
    ("--candidate", Arg.Set_string candidate, "candidate url");
    ("--reference", Arg.Set_string reference, "candidate url");
  ] in

  let rest path = paths := path::!paths in

  let usage = String.concat "\n" [
    "Filigrane : a useful REST API diff tool";
    "Usage: filigrane --candidate <candidate> --reference <reference> <path1> [<path2>] ..."
   ] in

  Arg.parse args rest usage;

  let handle_path path =
    compute_diff_result !candidate !reference path
  in

  let rec handle_paths paths =
    match paths with
    | head :: tail -> let* _ = handle_path head in handle_paths tail
    | [] -> Lwt.return_unit
  in

  handle_paths !paths

let () =
  Lwt_main.run main