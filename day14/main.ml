open Core
open Stdio

let input = In_channel.with_file "day14/input.txt" ~f:(fun file ->
  In_channel.input_lines file)

let template = List.hd_exn input |> String.to_list

let rules =
  let _, r = List.split_n input 2 in
  List.map r ~f:(fun s ->
    String.split_on_chars s ~on:[' '; '-'; '>']
    |> List.filter ~f:(fun s -> not @@ String.is_empty s)
    |> (fun l ->
        let fst = List.nth_exn l 0 in
        let snd = List.nth_exn l 1 in
        let a = String.get fst 0 in
        let b = String.get snd 0 in
        let c = String.get fst 1 in
        ( fst
        , [a; b] |> String.of_char_list
        , [b; c] |> String.of_char_list
        )
      ))

let pairs =
  List.fold template ~init:([], None) ~f:(fun (acc, last) ch ->
      match last with
      | None -> acc, Some ch
      | Some old_ch -> String.of_char_list [old_ch; ch] :: acc, Some ch)
  |> Tuple2.get1
  |> List.map ~f:(fun x -> x, 1)

let m =
  Map.of_alist_reduce (module String) pairs ~f:(+)

let rec apply m rules n =
  match n = 0 with
  | true  -> m
  | false ->
  let m' = Map.fold m ~init:[] ~f:(fun ~key:x ~data:n acc ->
      let _, y, z =
        List.find_exn rules ~f:(fun (t, _, _) -> let open String in x = t)
      in
      (y, n) :: (z, n) :: acc)
  |> Map.of_alist_reduce (module String) ~f:(+)
  in
  apply m' rules (n - 1)

let m10 = apply m rules 10

let m40 = apply m rules 40

let score_map m =
  Map.fold m ~init:[] ~f:(fun ~key:x ~data:n acc ->
      let a, b =
        String.get x 0 |> String.of_char,
        String.get x 1 |> String.of_char
      in
      (a, n) :: (b, n) :: acc)
  |> Map.of_alist_reduce (module String) ~f:(+)
  |> Map.map ~f:(fun v ->
      v // 2
      |> Float.round_up
      |> Int.of_float)

let max_value m : int =
  score_map m
  |> Map.to_alist
  |> List.map ~f:Tuple2.get2
  |> List.max_elt ~compare:Int.compare
  |> Option.value ~default:0

let min_value m : int =
  score_map m
  |> Map.to_alist
  |> List.map ~f:Tuple2.get2
  |> List.min_elt ~compare:Int.compare
  |> Option.value ~default:0

let max10, min10 =
  max_value m10,
  min_value m10

let max40, min40 =
  max_value m40,
  min_value m40

let () =
  printf "(max - min) for 10 steps: %d\n" @@ max10 - min10;
  printf "(max - min) for 40 steps: %d\n" @@ max40 - min40
