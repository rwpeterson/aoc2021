open Core
open Stdio

let input = In_channel.with_file "day12/input.txt" ~f:(fun file ->
  In_channel.input_lines file)

let rules =
  List.map input ~f:(fun line ->
    String.split line ~on:'-'
    |> fun x -> match x with a :: b :: [] -> (a, b) | _ -> ("", ""))
  |> List.fold ~init:[] ~f:(fun acc (x, y) ->
    match (x, y) with
    | ("start", _)
    | (_,   "end") -> (x, y) :: acc
    | (_, "start")
    | ("end",   _) -> (y, x) :: acc
    | _ -> (x, y) :: (y, x) :: acc)

let is_smol x =
  match x with
    | "start" -> false
    | "end"   -> false
    | x       -> String.(=) x (String.lowercase x)

let next_steps rules path doubles =
  let open String in
  let node = List.hd_exn path in
  List.fold rules ~init:[] ~f:(fun acc (x, y) ->
      match x = node with
      | true  -> begin
          let smol_dups =
            List.find_all_dups path ~compare:compare
            |> List.filter ~f:is_smol
            |> List.map ~f:(fun d -> d, List.count path ~f:(fun e -> e = d))
          in
          let single_smols =
            List.fold path ~init:[] ~f:(fun acc p ->
                match is_smol p && not @@ List.mem acc p ~equal:(=) with
                | true  -> p :: acc
                | false ->      acc)
          in
          let num_doubles =
            List.count smol_dups ~f:(fun (_, n) -> Int.(=) n 2)
          in
          match is_smol y with
          | true ->
            if Int.(=) num_doubles doubles then begin
              match List.mem single_smols y ~equal:(=) with
              | true  -> acc
              | false -> y :: acc
            end
            else begin
              y :: acc
            end
          | false -> y :: acc
        end
      | false -> acc)

let rec paths' rules lasts d =
  let nexts = List.fold lasts ~init:[] ~f:(fun acc last ->
      match last with
      | First path ->
        let nexts = next_steps rules path d in
        begin match nexts with
          | [] -> acc
          | x ->
            List.append
              (List.fold x ~init:[] ~f:(fun acc' node ->
                  match node with
                  | "end" -> Second (node :: path) :: acc'
                  | _     -> First  (node :: path) :: acc'))
              (acc)
        end
      | Second _ -> last :: acc) in
  let finished = List.fold nexts ~init:true ~f:(fun acc next ->
      match acc with
      | false -> false
      | true ->
        begin
          match next with
          | First _  -> false
          | Second _ -> true
        end) in
  match finished with
  | true  -> nexts
  | false -> paths' rules nexts d

let paths rules d =
  let final = paths' rules [First ["start"]] d in
  List.map final ~f:Either.value

let num_once_paths =
  List.length @@ paths rules 0

let num_twice_paths =
  List.length @@ paths rules 1

let () = printf "Revisit smol never paths: %d\n" num_once_paths
let () = printf "Revisit smol once paths: %d\n" num_twice_paths
