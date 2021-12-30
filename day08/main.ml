open Core
open Stdio

module Segment = struct
  module T = struct
    type t = A | B | C | D | E | F | G
    [@@deriving compare, sexp_of, hash]
  end
  include T
  include Comparator.Make(T)

  exception Invalid_segment

  let of_char ch = match ch with
  | 'a' -> Some A
  | 'b' -> Some B
  | 'c' -> Some C
  | 'd' -> Some D
  | 'e' -> Some E
  | 'f' -> Some F
  | 'g' -> Some G
  | _ -> None

  let of_char_exn ch = match of_char ch with
    | Some x -> x
    | None -> raise Invalid_segment

  let to_char sg = match sg with
    | A -> 'a'
    | B -> 'b'
    | C -> 'c'
    | D -> 'd'
    | E -> 'e'
    | F -> 'f'
    | G -> 'g'

end

let input = In_channel.with_file "day08/input.txt" ~f:(fun file ->
  In_channel.input_lines file)

let entries =
  List.map input ~f:(String.lsplit2_exn ~on:'|')
  |> List.map ~f:(fun (x, y) ->
      ( String.split x ~on:' ' |> List.filter ~f:(fun x -> not (String.is_empty x))
      , String.split y ~on:' ' |> List.filter ~f:(fun y -> not (String.is_empty y))
      ))

let segment_set pattern n =
    List.filter pattern ~f:(fun s -> String.length s = n)
    |> List.hd_exn
    |> String.to_list
    |> List.map ~f:Segment.of_char_exn
    |> Set.of_list (module Segment)

let segment_set_all pattern n =
    List.filter pattern ~f:(fun s -> String.length s = n)
    |> List.map ~f:String.to_list
    |> List.map ~f:(List.map ~f:(Segment.of_char_exn))
    |> List.map ~f:(Set.of_list (module Segment))

let segment_set_with pattern n other =
    segment_set_all pattern n
    |> List.filter ~f:(fun x -> Set.is_subset other ~of_:x)
    |> List.hd_exn

let segment_set_without pattern n other =
    segment_set_all pattern n
    |> List.filter ~f:(fun x -> not (Set.is_subset other ~of_:x))
    |> List.hd_exn

let string_of_set s = Set.fold s ~init:[] ~f:(fun acc x ->
    let y = Segment.to_char x in
    y :: acc)
    |> List.sort ~compare:Char.compare
    |> String.of_char_list

let code_table (patterns : string list) =
  let one   = segment_set patterns 2 in
  let four  = segment_set patterns 4 in
  let seven = segment_set patterns 3 in
  let eight = segment_set patterns 7 in
  let a = Set.diff seven one in
  let six = segment_set_without patterns 6 one in
  let c = Set.diff one six in
  let f = Set.diff one c in
  let three = segment_set_with patterns 5 one in
  let e = Set.diff (Set.diff six four) three in
  let b = Set.diff (Set.diff six three) e in
  let nine = Set.diff eight e in
  let five = Set.diff nine c in
  let two = Set.diff (Set.diff eight b) f in
  let g = Set.diff (Set.diff (Set.diff eight four) a) e in
  let zero = Set.union (Set.union (Set.union seven b) e) g in
  let _d = Set.diff eight zero in
  let h = Hashtbl.create (module String) in
  Hashtbl.set h ~key:(string_of_set one  ) ~data:1;
  Hashtbl.set h ~key:(string_of_set two  ) ~data:2;
  Hashtbl.set h ~key:(string_of_set three) ~data:3;
  Hashtbl.set h ~key:(string_of_set four ) ~data:4;
  Hashtbl.set h ~key:(string_of_set five ) ~data:5;
  Hashtbl.set h ~key:(string_of_set six  ) ~data:6;
  Hashtbl.set h ~key:(string_of_set seven) ~data:7;
  Hashtbl.set h ~key:(string_of_set eight) ~data:8;
  Hashtbl.set h ~key:(string_of_set nine ) ~data:9;
  Hashtbl.set h ~key:(string_of_set zero ) ~data:0;
  h

let segment_sets_of_strings (values : string list) =
    List.map values ~f:String.to_list
    |> List.map ~f:(List.map ~f:(Segment.of_char_exn))
    |> List.map ~f:(Set.of_list (module Segment))

let decode' h msgs = List.map msgs ~f:(fun msg ->
  let string_msg = string_of_set msg in
  Hashtbl.find_exn h string_msg)

let decode patterns values = decode' (code_table patterns) (segment_sets_of_strings values)

let decoded_messages = List.map entries ~f:(fun (patterns, values) -> decode patterns values)

let spec_digits = List.fold decoded_messages ~init:0 ~f:(fun acc msg ->
    let y = List.fold msg ~init:0 ~f:(fun acc' x ->
        match x with
        | 1 | 4 | 7 | 8 -> acc' + 1
        | _ -> acc'
      ) in
    acc + y)

let total_sum = List.fold decoded_messages ~init:0 ~f:(fun acc msg ->
    let y, _ = List.rev msg |> List.fold ~init:(0, 0) ~f:(fun (acc, pow) d ->
      (acc + d * (Int.pow 10 pow), pow + 1)
      ) in
    acc + y
  )

let () =
  printf "Number of 1478 digit appearances: %d\n" spec_digits;
  printf "Sum of all decoded numbers: %d\n" total_sum
