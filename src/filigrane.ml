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
    match candidate |> List.find_opt (Json.equal c) with 
    | Some e -> diff_json c e
    | None -> Some(MissingArrayElem i)
  in
  let diff_list = reference
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
    match candidate |> List.find_opt (find k) with
    | Some (_, vv) -> diff_json v vv |> Option.map(fun (x) -> (k,x))
    | None -> Some((k,MissingObjectKey k))
  in
  let diff_list = reference
    |> List.filter_map step 
  in
  if List.length diff_list > 0 then
    Some(ObjectDiff diff_list)
  else
    None

let relay_request scheme host req body =
  let meth    = req |> Request.meth in 
  let headers = Cohttp.Header.add req.headers "Host" host |> Cohttp.Header.clean_dup in
  let uri     = [scheme; "://"; host; req.resource] |> String.concat "" |> Uri.of_string in
  print_endline (String.concat " " [Cohttp.Code.string_of_method meth; Uri.to_string uri]);
  Client.call ~headers ~body:body meth uri

let decode_body_json body =
  let* body_str = body |> Cohttp_lwt.Body.to_string in 
  body_str |> Json.from_string |> Lwt.return

let server candidate reference scheme port =
  let callback _conn req body =

    let* (_, candidate_body) = relay_request scheme candidate req body
    and* (r, reference_body) = relay_request scheme reference req body in

    let* candidate_json = decode_body_json candidate_body
    and* reference_json = decode_body_json reference_body in
    
    let diff = diff_json candidate_json reference_json in
    let json = json_of_diff_result {path = req.resource; diff} in
    
    let body = json
      |> Json.to_string 
      |> Cohttp_lwt.Body.of_string in

    Server.respond ~status:r.status ~body ()
  in
    Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())


let main =
  let port = ref 8000 in
  let scheme = ref "https" in
  let candidate = ref "" in
  let reference = ref "" in
  let rest = ref [] in

  let args = [
    ("--port", Arg.Set_int port, "port to expose: default 8000");
    ("--scheme", Arg.Set_string scheme, "scheme to use (one of http, https. default https)");
    ("--candidate", Arg.Set_string candidate, "candidate host");
    ("--reference", Arg.Set_string reference, "candidate host");
  ] in

  let rest path = rest := path::!rest in

  let usage = String.concat "\n" [
    "Filigrane : a useful REST API diff tool";
    "Usage: filigrane --candidate <candidate> --reference <reference>"
   ] in

  Arg.parse args rest usage;
  
  Printf.printf "Filigrane proxy server starting on port %d\n" !port;
  Printf.printf "Scheme : %s\n" !scheme;
  Printf.printf "Reference : %s\n" !reference;
  Printf.printf "Candidate : %s\n" !candidate;
  flush stdout;

  server !candidate !reference !scheme !port

let () =
  Lwt_main.run main