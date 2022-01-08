open Core
open Stdio

let input = In_channel.with_file "day15/input.txt" ~f:(fun file ->
  In_channel.input_lines file)

let d = List.length input
let d2 = 5 * (List.length input)

let edges = Array.make_matrix ~dimx:d ~dimy:d 0
let edges2 = Array.make_matrix ~dimx:d2 ~dimy:d2 0

let () =
  List.iteri input ~f:(fun i s ->
      List.iteri (String.to_list s) ~f:(fun j ch ->
          let x = String.of_char ch |> Int.of_string in
          edges.(i).(j) <- x))

let tiles x y =
  List.fold (List.range 0 5) ~init:[] ~f:(fun acc i ->
    List.append
      (List.map (List.range 0 5) ~f:(fun j ->
        (x + i * d, y + j * d, i, j)))
      acc)

let () =
  List.iteri input ~f:(fun i s ->
      List.iteri (String.to_list s) ~f:(fun j ch ->
          let z = String.of_char ch |> Int.of_string in
          List.iter (tiles i j) ~f:(fun (k, l, ii, jj) ->
            let z' = ((z + ii + jj - 1) mod 9) + 1 in
            edges2.(k).(l) <- z')))

module P = Pairing_heap
module H = Hashtbl.Poly

let qcmp a b =
  let x = Tuple3.get3 a in
  let y = Tuple3.get3 b in
  Int.compare x y

let mk_val (i, j) v = (i, j, v)

let get_point (a, b, _) = (a, b)

let is_point (i, j) (a, b, _) = i = a && j = b

let neighbors (i, j) d =
  let l =
    [ (i - 1, j    )
    ; (i    , j - 1)
    ; (i    , j + 1)
    ; (i + 1, j    )
    ] in
  List.fold l ~init:[] ~f:(fun acc (x, y) ->
      match x < 0 || x >= d || y < 0 || y >= d with
      | true  -> acc
      | false -> (x, y) :: acc)

let dijkstra start finish edges =
  let d = Array.length edges in
  let i0, j0 = Tuple2.get1 start, Tuple2.get2 start in
  let dist = H.create () in
  H.set dist ~key:(0, 0) ~data:0;
  let prev = H.create () in
  let qtkn = H.create () in
  let queue = P.create ~cmp:qcmp () in
  Array.iteri edges ~f:(fun i _ ->
    Array.iteri edges.(i) ~f:(fun j _ ->
      if i <> i0 || j <> j0 then begin
        H.set dist ~key:(i, j) ~data:Int.max_value;
        H.set prev ~key:(i, j) ~data:None;
      end));
  H.set qtkn ~key:(0, 0) ~data:(P.add_removable queue (0, 0, H.find_exn dist (0, 0)));

  while (not @@ P.is_empty queue) do
    let u = P.pop_exn queue |> get_point in
    List.iter (neighbors u d) ~f:(fun (i, j) ->
        let alt = (H.find_exn dist u) + edges.(i).(j) in
        match P.find queue ~f:(fun a -> is_point (i, j) a) with
        | None ->
          let v = i, j in begin
          try
          if alt < (H.find_exn dist v) then begin
            H.set dist ~key:v ~data:alt;
            H.set prev ~key:v ~data:(Some u);
            H.set qtkn ~key:v ~data:(
              P.add_removable
                queue
                (mk_val v alt))
          end;
          with
          | Not_found_s _ -> printf "LOL at i, j: %d, %d\n" i j;
        end
        | Some a ->
          let v = get_point a in
          if alt < (H.find_exn dist v) then begin
            H.set dist ~key:v ~data:alt;
            H.set prev ~key:v ~data:(Some u);
            H.set qtkn ~key:v ~data:(
              P.update
                queue
                (H.find_exn qtkn v)
                (mk_val v alt))
          end;)
  done;
  H.find_exn dist finish

let () = printf "Lowest risk of any path: %d\n" @@ dijkstra (0, 0) (d - 1, d - 1) edges
let () = printf "Lowest risk of any path (v2): %d\n" @@ dijkstra (0, 0) (d2 - 1, d2 - 1) edges2
