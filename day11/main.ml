open Core
open Stdio

let input = In_channel.with_file "day11/input.txt" ~f:(fun file ->
  In_channel.input_lines file)

let dimy = List.hd_exn input |> String.length
let dimx = List.length input

let grid = Array.make_matrix ~dimx:dimx ~dimy:dimy 0

let reset_grid grid = List.iter (List.range 0 dimx) ~f:(fun i ->
List.iter (List.range 0 dimy) ~f:(fun j ->
    let x = List.nth_exn input i
            |> String.to_list
            |> (fun z -> List.nth_exn z j)
            |> String.of_char
            |> Int.of_string
    in
    grid.(i).(j) <- x))

let () = reset_grid grid

(* this time with diagonals *)
let neighbors i j m =
  let dimx = Array.length m.(0) in
  let dimy = Array.length m in
  let l =
    [ (i - 1, j - 1)
    ; (i - 1, j    )
    ; (i - 1, j + 1)
    ; (i    , j - 1)
    ; (i    , j + 1)
    ; (i + 1, j - 1)
    ; (i + 1, j    )
    ; (i + 1, j + 1)
    ] in
  List.fold l ~init:[] ~f:(fun acc (x, y) ->
      match x < 0 || x >= dimx || y < 0 || y >= dimy with
      | true  -> acc
      | false -> (x, y) :: acc)

let rec update' grid first acc =
  let dimx = Array.length grid.(0) in
  let dimy = Array.length grid in
  if first then begin
    List.iter (List.range 0 dimx) ~f:(fun i ->
      List.iter (List.range 0 dimy) ~f:(fun j ->
        let x = grid.(i).(j) in
        grid.(i).(j) <- x + 1));
  end;
  let m = ref 0 in
  List.iter (List.range 0 dimx) ~f:(fun i ->
    List.iter (List.range 0 dimy) ~f:(fun j ->
      if grid.(i).(j) > 9 then begin
        Int.incr m;
        grid.(i).(j) <- 0;
        let n = neighbors i j grid in
        List.iter n ~f:(fun (k, l) ->
          let y = grid.(k).(l) in
          if y > 0 then begin
            grid.(k).(l) <- y + 1
          end)
      end));
  match !m > 0 with
  | true  -> update' grid false (acc + !m)
  | false -> acc

let update grid = update' grid true 0

let flashes = List.fold (List.range 0 100) ~init:0 ~f:(fun a _ -> a + (update grid))

let () = printf "Flashes after 100 steps: %d\n" flashes

let is_synchronized grid =
  let m = ref 0 in
  Array.iteri grid ~f:(fun i _ ->
      Array.iteri grid.(i) ~f:(fun j _ ->
          if grid.(i).(j) > 0 then begin
            Int.incr m
          end));
  !m = 0

let rec synchro grid n =
  let _: int = update grid in
  match is_synchronized grid with
  | true  -> n + 1
  | false -> synchro grid n + 1

let () = reset_grid grid

let sync_step = synchro grid 0

let () = printf "Synchronization at step %d\n" sync_step
