open Core
open Stdio

let len = 12

let input = In_channel.with_file "day03a/input.txt" ~f:(fun file ->
  In_channel.fold_lines file ~init:[] ~f:(fun xs x ->
    Int.of_string ("0b" ^ x) :: xs))

let gamma' xs b = let a = List.fold xs ~init:0 ~f:(fun acc x ->
    match (x lsr b) land 1 = 1 with
    | true  -> acc + 1
    | false -> acc - 1) in
        match a >= 0 with
        | true -> 1 lsl b
        | false -> 0

let gamma = List.fold
    (List.map (List.range 0 len) ~f:(fun b -> gamma' input b))
    ~init:0
    ~f:(+)

let mask = 0b111111111111

let epsilon = lnot gamma land mask

let rec oxygen' xs b =
  match b with
  | 0 -> xs
  | _ -> let a = List.fold xs ~init:0 ~f:(fun acc x ->
    match (x lsr b) land 1 = 1 with
    | true -> acc + 1
    | false -> acc - 1) in
      match a >= 0 with
      | true  -> oxygen' (List.filter xs ~f:(fun x -> (x lsr b) land 1 = 1)) (b - 1)
      | false -> oxygen' (List.filter xs ~f:(fun x -> (x lsr b) land 1 = 0)) (b - 1)

let oxygen = List.hd_exn (oxygen' input (len - 1))

let rec co2' xs b =
  match List.length xs with
  | 1 -> xs
  | _ ->
  match b with
  | 0 -> xs
  | _ -> let a = List.fold xs ~init:0 ~f:(fun acc x ->
    match (x lsr b) land 1 = 1 with
    | true  -> acc + 1
    | false -> acc - 1) in
      match a >= 0 with
      | true  -> co2' (List.filter xs ~f:(fun x -> (x lsr b) land 1 = 0)) (b - 1)
      | false -> co2' (List.filter xs ~f:(fun x -> (x lsr b) land 1 = 1)) (b - 1)

let co2 = List.hd_exn (co2' input (len - 1))

let () =
  printf "gamma * epsilon = %d * %d = %d\n" gamma epsilon (gamma * epsilon)

let () =
  printf "oxygen * co2 = %d * %d = %d\n" oxygen co2 (oxygen * co2)
