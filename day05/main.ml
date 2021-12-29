open Core
open Stdio

let input = In_channel.with_file "day05/input.txt" ~f:(fun file ->
  In_channel.input_lines file)

let coords = List.map input ~f:(fun l ->
  String.split_on_chars l ~on:[' ';'-';'>']
  |> List.filter_map ~f:(function "" -> None | s -> Some(s))
  |> List.map ~f:(String.split ~on:',')
  |> fun x ->
    ( Int.of_string (List.nth_exn (List.nth_exn x 0) 0)
    , Int.of_string (List.nth_exn (List.nth_exn x 0) 1)
    , Int.of_string (List.nth_exn (List.nth_exn x 1) 0)
    , Int.of_string (List.nth_exn (List.nth_exn x 1) 1)
    )
  )

let x1max = List.fold coords ~init:0 ~f:(fun acc (x, _, _, _) -> Int.max acc x)
let x2max = List.fold coords ~init:0 ~f:(fun acc (_, _, x, _) -> Int.max acc x)
let y1max = List.fold coords ~init:0 ~f:(fun acc (_, y, _, _) -> Int.max acc y)
let y2max = List.fold coords ~init:0 ~f:(fun acc (_, _, _, y) -> Int.max acc y)

(* this tuple was not quite the right structure here... quite verbose *)

let dimx = Int.max x1max x2max
let dimy = Int.max y1max y2max

let horiz_or_vert (x1, y1, x2, y2) = x1 = x2 || y1 = y2

let count_overlap m =
  let ct = ref 0 in
  List.iter (List.range 0 (dimx + 1)) ~f:(fun x ->
      List.iter (List.range 0 (dimy + 1)) ~f:(fun y ->
        if m.(x).(y) >= 2 then ct := !ct + 1));
  !ct

let mat_incr m x y =
  let v = m.(x).(y) in
  m.(x).(y) <- v + 1

(* Some trajectories are backwards but we iterate forwards *)
let make_range_incl z1 z2 =
  match z1 < z2 with
  | true  -> List.range z1 (z2 + 1)
  | false -> List.range z2 (z1 + 1)

let make_diag_incl x1 y1 x2 y2 =
  let xs = match x1 < x2 with
    | true  -> make_range_incl x1 x2
    | false -> make_range_incl x1 x2 |> List.rev in
  let ys = match y1 < y2 with
    | true  -> make_range_incl y1 y2
    | false -> make_range_incl y1 y2 |> List.rev in
  List.zip_exn xs ys

let grid = Array.make_matrix ~dimx:(dimx + 1) ~dimy:(dimy + 1) 0

let () = List.iter coords
    ~f:(fun (x1, y1, x2, y2) ->
        match horiz_or_vert (x1, y1, x2, y2) with
        | true  -> begin match x1 = x2 with
          | true  -> List.iter (make_range_incl y1 y2) ~f:(fun y -> mat_incr grid x1 y)
          | false -> List.iter (make_range_incl x1 x2) ~f:(fun x -> mat_incr grid x y1)
          end
        | false -> ()
      )

let () =
  printf "Sites with overlap: %d\n" (count_overlap grid)

let () = List.iter coords
    ~f:(fun (x1, y1, x2, y2) ->
        match horiz_or_vert (x1, y1, x2, y2) with
        | true  -> ()
        | false -> List.iter (make_diag_incl x1 y1 x2 y2) ~f:(fun (x, y) -> mat_incr grid x y)
      )

let () = printf "Sites with overlap (counting diag): %d\n" (count_overlap grid)
