open Core
open Stdio

let input = In_channel.with_file "day04a/input.txt" ~f:(fun file ->
  In_channel.input_lines file)

let draws = List.hd_exn input |> String.split ~on:',' |> List.map ~f:Int.of_string

let string_to_row l = String.split l ~on:' ' |> (List.filter_map ~f:(fun x ->
    match String.is_empty x with
    | false -> Some (Int.of_string x)
    | true  -> None))

let cards = List.tl_exn input |> List.fold ~init:([], []) ~f:(fun (acc, tmp) l ->
    match String.is_empty l with
    | true  -> (acc, tmp)
    | false -> match List.length tmp with
      | 4 -> (string_to_row l :: tmp) :: acc, []
      | _ -> acc, (string_to_row l) :: tmp) |> (fun (x, _) -> List.map x ~f:(fun y -> List.rev y))

let winrow card draws = List.fold card ~init:false ~f:(fun acc row ->
    match acc with
    | true -> true
    | false -> List.fold row ~init:true ~f:(fun r_acc x ->
        match r_acc with
        | false -> false
        | true -> List.mem draws x ~equal:(=)))

let wincol card draws = winrow (Option.value_exn (List.transpose card)) draws

let windiag1 card draws =
  let win, _ = List.fold card ~init:(true, 0) ~f:(fun (acc, coli) row ->
  match acc with
    | false -> (acc, coli + 1)
    | true  -> match List.find draws ~f:(fun x -> x = List.nth_exn row coli) with
      | Some _ -> (true, coli + 1)
      | None   -> (false, coli + 1)) in
  win

let windiag2 card draws = windiag1 (Option.value_exn (List.transpose card)) draws

let windiag card draws = (windiag1 card draws) || (windiag2 card draws)

let winning card draws = (winrow card draws) || (wincol card draws) || (windiag card draws)

let flatten list =
  let rec aux accu = function
    | []            -> accu
    | []       :: t -> aux accu t
    | (x :: y) :: t -> aux (x :: accu) (y :: t) in
  List.rev (aux [] list)

let rec find_index' xs x c = match xs with
  | [] -> None
  | hd::tl -> match hd = x with
    | true -> Some c
    | false -> find_index' tl x (c + 1)

let find_index xs x = find_index' xs x 0

let unmarked_sum card draws = List.filter (flatten card) ~f:(fun x ->
  Option.is_none (List.find draws ~f:(fun y -> y = x))) |> List.fold ~init:0 ~f:(+)

let scored card draws = unmarked_sum card draws |> ( * ) (List.hd_exn draws)

let cumulative_draws = List.fold draws ~init:[] ~f:(fun acc d ->
    match acc with
    | []     -> (d :: []) :: []
    | hd::tl -> (d :: hd) :: hd :: tl)
                       |> List.rev

let winner' = List.fold cumulative_draws ~init:None ~f:(fun acc draws' ->
    match acc with
    | Some x -> Some x
    | None   ->
      match List.find cards ~f:(fun card -> winning card draws') with
      | Some c -> Some (c, draws')
      | None   -> None)

let wincard, windraws = Option.value_exn winner'

let last_winning_card = let last = List.fold cumulative_draws ~init:cards ~f:(fun acc draws ->
    match acc with
      | hd :: [] -> hd :: []
      | _ -> List.filter acc ~f:(fun card ->
          not (winning card draws))) in
    List.hd_exn last


let last_winning_score = Option.value_exn (List.fold cumulative_draws ~init:None ~f:(fun acc draws ->
    match acc with
    | Some x -> Some x
    | None   -> match winning last_winning_card draws with
      | true  -> Some (scored last_winning_card draws)
      | false -> None))


let () =
  printf "winning card:\n";
  List.iter wincard ~f:(fun row -> List.iter row ~f:(fun x -> printf "%d " x); printf "\n");
  printf "sum of unmarked numbers: %d\n" (unmarked_sum wincard windraws);
  printf "winning draw: %d\n" (List.hd_exn windraws);
  printf "winning score: %d\n" (unmarked_sum wincard windraws * (List.hd_exn windraws));
  printf "last winning score: %d\n" last_winning_score;
