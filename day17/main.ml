open Core
open Stdio

let input = In_channel.read_all "day17/input.txt"

let dim =
  Tyre.(
    str"target area: x=" *> int <&>
    str".." *> int <&>
    str", y=" *> int <&>
    str".." *> int
  )

let dim_re = Tyre.compile dim

let ((x1, x2), y1), y2 = Tyre.exec dim_re input |> Caml.Result.get_ok
let target = (x1, x2, y1, y2)

let step (x, y) (u, v) =
  let (x', y') = (x + u, y + v) in
  let u' = match Int.sign u with
    | Sign.Pos  -> u - 1
    | Sign.Zero -> 0
    | Sign.Neg  -> u + 1 in
  let v' = v - 1 in
  (x', y'), (u', v')

let in_target (x, y) (x1, x2, y1, y2) =
  x1 <= x && x <= x2 && y1 <= y && y <= y2

let beyond_target (x, y) (_x1, x2, y1, _y2) =
  x2 < x || y < y1

type trajectory = Flying of int | Hit of int | Miss

let update_traj (x, y) traj target =
  match traj with
  | Miss  -> Miss
  | Hit z -> Hit z
  | Flying z ->
    if beyond_target (x, y) target
    then Miss
    else if in_target (x, y) target
    then Hit z
    else begin
      match y > z with
      | true  -> Flying y
      | false -> Flying z
    end

let rec fire (x, y) (u, v) t target =
  let (x', y'), (u', v') = step (x, y) (u ,v) in
  let t' = update_traj (x', y') t target in
  match t' with
  | Miss -> None
  | Hit z -> Some z
  | Flying _ -> fire (x', y') (u', v') t' target


let d = 300

let peak = ref 0
let good = ref 0
let () =
  List.iter (List.range 0 d) ~f:(fun i ->
    List.iter (List.range (-d) d) ~f:(fun j ->
      let z = fire (0, 0) (i, j) (Flying 0) target in
        match z with
        | Some z' -> if z' > !peak then (peak := z'); incr good
        | None -> ()))



let () = printf "Max height for successful hit: %d\n" !peak
let () = printf "Number of good initial conditions: %d\n" !good
