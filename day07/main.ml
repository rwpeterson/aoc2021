open Core
open Stdio

let input = In_channel.with_file "day07/input.txt" ~f:(fun file ->
  In_channel.input_all file)

let positions = String.strip input |> String.split ~on:',' |> List.map ~f:Int.of_string

let max = List.fold positions ~init:0 ~f:Int.max
let min = List.fold positions ~init:0 ~f:Int.min

let candidates = List.range min max

let costs = List.map candidates ~f:(fun x ->
    List.fold positions ~init:0 ~f:(fun acc y ->
      acc + (Int.abs (x - y))))

let costs2 = List.map candidates ~f:(fun x ->
    List.fold positions ~init:0 ~f:(fun acc y ->
        let z = Int.abs (x - y) in
        let c = z * (z + 1) / 2 in
        acc + c))

let best_strat candidates costs = let open List in
  Option.value_exn (fold (zip_exn candidates costs) ~init:None ~f:(fun acc (x, c) ->
      match acc with
      | None -> Some (x, c)
      | Some (y, d) -> begin match d < c with
          | true  -> Some (y, d)
          | false -> Some (x, c)
        end))

let optimal_position, optimal_cost = best_strat candidates costs

let optimal_position_2, optimal_cost_2 = best_strat candidates costs2

let () = printf "Optimal fuel strategy - position: %d, cost: %d\n" optimal_position optimal_cost
let () = printf "Optimal fuel strategy 2 - position: %d, cost: %d\n" optimal_position_2 optimal_cost_2
