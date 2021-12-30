open Core
open Stdio

let input = In_channel.with_file "day09/input.txt" ~f:(fun file ->
  In_channel.input_lines file)


let dimy = List.hd_exn input |> String.length
let dimx = List.length input

let grid = Array.make_matrix ~dimx:dimx ~dimy:dimy 0

let () = List.iter (List.range 0 dimx) ~f:(fun i ->
    List.iter (List.range 0 dimy) ~f:(fun j ->
        let x = List.nth_exn input i
                |> String.to_list
                |> (fun z -> List.nth_exn z j)
                |> String.of_char
                |> Int.of_string
        in
        grid.(i).(j) <- x))

let neighbors i j =
  let xs = match i = 0 with
  | true  -> [i + 1]
  | false -> begin match i = dimx - 1 with
      | true  -> [i - 1]
      | false -> [i - 1; i + 1]
    end in
  let ys = match j = 0 with
  | true  -> [j + 1]
  | false -> begin match j = dimy - 1 with
      | true  -> [j - 1]
      | false -> [j - 1; j + 1]
    end in
  List.append
    (List.map xs ~f:(fun x -> (x, j)))
    (List.map ys ~f:(fun y -> (i, y)))

let is_low grid i j =
  List.fold (neighbors i j) ~init:true ~f:(fun acc (k, l) ->
      match acc with
      | true -> grid.(i).(j) < grid.(k).(l)
      | false -> false
    )

let get_lows grid = Array.foldi grid ~init:[] ~f:(fun i acc row ->
    let rowlist = Array.foldi row ~init:[] ~f:(fun j acc' x ->
        match is_low grid i j with
        | true -> (i, j, x) :: acc'
        | false -> acc'
      ) in
    List.append rowlist acc)

let low_points = get_lows grid

let risk_level x = 1 + x

let total_risk = List.fold low_points ~init:0 ~f:(fun acc (_, _, h) ->
  acc + (risk_level h))

let () = printf "Total risk of low points: %d\n" total_risk

let basins = Array.make_matrix ~dimx:dimx ~dimy:dimy None

let rec descend grid i j =
  match grid.(i).(j) = 9 with
  | true  -> None
  | false ->
  let n = neighbors i j in
  let i', j', _ = List.fold n ~init:(None, None, None) ~f:(fun (x0, y0, acc) (x, y) ->
      let q = grid.(x).(y) in
      match acc with
      (* If there is a neighbor under consideration, we'll take a better one *)
      | Some a -> begin match q < a with
          | true  -> (Some x, Some y, Some q)
          | false -> (x0, y0, acc)
        end
      (* If there is not a neighbor under consideration, compare to self too *)
      | None -> let p = grid.(i).(j) in
        begin match q < p with
          | true  -> (Some x, Some y, Some q)
          | false -> (Some i, Some j, Some p)
        end
    ) in
  let i'', j'' = Option.value_exn i', Option.value_exn j' in
  match Tuple2.equal ~eq1:(=) ~eq2:(=) (i, j) (i'', j'') with
  | true  -> Some (i'', j'')
  | false -> descend grid i'' j''

let () = List.iter (List.range 0 dimx) ~f:(fun i ->
    List.iter (List.range 0 dimy) ~f:(fun j ->
        basins.(i).(j) <- descend grid i j))

module Basin = struct
  module T = struct
    type t = (int * int) option
    [@@deriving compare, sexp_of, hash]
  end
  include T
  include Comparator.Make(T)
end

let h = Hashtbl.create (module Basin)

let () = List.iter (List.range 0 dimx) ~f:(fun i ->
    List.iter (List.range 0 dimy) ~f:(fun j ->
        Hashtbl.change h basins.(i).(j) ~f:(fun d -> match d with
            | Some v -> Some (v + 1)
            | None   -> Some 1
          )))

let basin_counts = Hashtbl.fold h ~init:[] ~f:(fun ~key:k ~data:d acc ->
    match k with
    | None -> acc
    | Some (_, _) -> d :: acc
  )

let product_three =
  List.sort basin_counts ~compare:Int.compare
  |> List.rev
  |> (fun x -> List.take x 3)
  |> List.fold ~init:1 ~f:( * )

let () = printf "Sum of three largest basins: %d\n" product_three
