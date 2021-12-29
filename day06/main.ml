open Core
open Stdio

let input = In_channel.with_file "day06/input.txt" ~f:(fun file ->
  In_channel.input_all file)

let initial_pop = String.strip input |> String.split ~on:',' |> List.map ~f:Int.of_string

let update_state (a : int array ref) =
  let accum = Array.foldi !a ~init:[] ~f:(fun i acc x ->
    match x with
    | 0 ->
        !a.(i) <- 6;
        [|8|] :: acc
    | _ ->
        !a.(i) <- x - 1;
        acc
    ) in
    let total = !a :: accum in
    a := Array.concat total

let pop = ref (List.map initial_pop ~f:(fun x -> [|x|]) |> Array.concat)

let () = for _ = 1 to 80 do
    update_state pop
  done

let () = printf "population size (80 days) %d\n" (Array.length !pop)

(* fun's over, we need to stop messing around and use a hash table *)
(* instead of letting memory grow exponentially *)

let h = ref (Hashtbl.create (module Int))

let update_hashtbl_state h = let open Hashtbl in
  let g = create (module Int) in
  let () = iteri !h ~f:(fun ~key:x ~data:n ->
      match x with
      | 0 -> begin
          change g 6 ~f:(fun y ->
            match y with
            | Some z -> Some (z + n)
            | None -> Some n
            );
          change g 8 ~f:(fun y ->
              match y with
              | Some z -> Some (z + n)
              | None -> Some n
            )
        end
      | _ -> change g (x - 1) ~f:(fun y ->
          match y with
          | Some z -> Some (z + n)
          | None -> Some n
        )) in
  h := g

let total_256 =
  List.iter initial_pop ~f:(fun x ->
      Hashtbl.change !h x ~f:(fun y ->
          match y with
          | Some z -> Some (z + 1)
          | None -> Some 1
        ));
  for _ = 1 to 256 do
    update_hashtbl_state h
  done;
  Hashtbl.fold !h ~init:0 ~f:(fun ~key:_ ~data:x acc -> acc + x)

let () = printf "population size (256 days) %d\n" total_256
