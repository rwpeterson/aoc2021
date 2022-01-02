open Core
open Stdio

module Fold = struct
  module T = struct
    type t = X | Y
  end
  include T

  let of_string = function
    | "x" | "X" -> Some X
    | "y" | "Y" -> Some Y
    | _         -> None

  let of_string_exn s = match of_string s with
    | Some x -> x
    | None   -> raise @@ Invalid_argument "Must be \"xyXY\" only"
end

let input = In_channel.with_file "day13/input.txt" ~f:(fun file ->
  In_channel.input_lines file)

let coords =
  List.take_while input ~f:(fun line -> String.contains line ',')
  |> List.map ~f:(fun line ->
      String.split line ~on:','
      |> (fun l -> (List.nth_exn l 0, List.nth_exn l 1))
      |> Tuple2.map ~f:Int.of_string)

let folds =
  List.filter input ~f:(fun line ->
    let open String.Search_pattern in
    matches (create "fold along ") line)
  |> List.map ~f:(fun line ->
    let open String.Search_pattern in
    replace_first (create "fold along ") ~in_:line ~with_:""
    |> String.split ~on:'='
    |> (fun l -> (List.nth_exn l 0, List.nth_exn l 1))
    |> Tuple2.map_fst ~f:Fold.of_string_exn
    |> Tuple2.map_snd ~f:Int.of_string)

let dimx = List.fold coords ~init:0 ~f:(fun acc (x, _) ->
    match x + 1 > acc with
    | true  -> x + 1
    | false -> acc)

let dimy = List.fold coords ~init:0 ~f:(fun acc (_, y) ->
    match y + 1 > acc with
    | true  -> y + 1
    | false -> acc)

let a = Array.make_matrix ~dimx:dimx ~dimy:dimy 0

let () = List.iter coords ~f:(fun (i, j) ->
  a.(i).(j) <- 1)

let b = Array.make_matrix ~dimx:dimx ~dimy:dimy 0

let () = List.iter coords ~f:(fun (i, j) ->
  b.(i).(j) <- 1)

let blit_cells a i1 j1 i2 j2 =
  let x = a.(i1).(j1) in
  if x = 1 then begin
    a.(i2).(j2) <- 1
  end

let foldy a j =
  let dimx = Array.length a in
  let dimy = Array.length a.(0) in
  List.iter (List.range 0 dimx) ~f:(fun i ->
    List.iter (List.range 1 (Int.min (j + 1) (dimy - j))) ~f:(fun k ->
      blit_cells a i (j + k) i (j - k)
    ))

let foldx a i =
  let dimx = Array.length a in
  let dimy = Array.length a.(0) in
  List.iter (List.range 1 (Int.min (i + 1) (dimx - i))) ~f:(fun l ->
    List.iter (List.range 0 dimy) ~f:(fun j ->
      blit_cells a (i + l) j (i - l) j
    ))

let make_folds a (folds : (Fold.t * int) list) =
  List.fold folds ~init:(dimx, dimy) ~f:(fun (z, w) (axis, n) ->
      match axis with
      | X -> begin
          let () = foldx a n in
          match n < z with
          | true  -> (n, w)
          | false -> (z, w)
        end
      | Y -> begin
          let () = foldy a n in
          match n < w with
          | true  -> (z, n)
          | false -> (z, w)
        end)

let count_inside a i j =
  let c = ref 0 in
  let () = List.iter (List.range 0 i) ~f:(fun i' ->
    List.iter (List.range 0 j) ~f:(fun j' ->
      if a.(i').(j') = 1 then Int.incr c))
  in !c

let vx, vy = make_folds a [List.hd_exn folds]

let () = printf "Dots visible after the first fold: %d\n" @@ count_inside a vx vy

let wx, wy = make_folds b folds

let c = Array.transpose_exn b

let () =
  List.iter (List.range 0 wy) ~f:(fun i ->
      List.iter (List.range 0 wx) ~f:(fun j ->
          printf "%s " @@ match c.(i).(j) = 1 with true -> "#" | _ -> ".");
      printf "\n"
    )
