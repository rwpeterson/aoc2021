open Core
open Stdio

let input = In_channel.with_file "day10/input.txt" ~f:(fun file ->
  In_channel.input_lines file)

module Bracket = struct
  module T = struct
    type t = Lpar | Rpar | Lbrk | Rbrk | Lbrc | Rbrc | Lche | Rche
    [@@deriving compare, sexp_of, hash]
  end
  include T
  include Comparator.Make(T)

  let pair x y = match (x, y) with
    | Lpar, Rpar -> true
    | Lbrk, Rbrk -> true
    | Lbrc, Rbrc -> true
    | Lche, Rche -> true
    |    _,    _ -> false

  let should_match x = match x with
    | Rpar -> Some Lpar
    | Rbrk -> Some Lbrk
    | Rbrc -> Some Lbrc
    | Rche -> Some Lche
    | _    -> None

  let cost x = match x with
    | Rpar -> Some 3
    | Rbrk -> Some 57
    | Rbrc -> Some 1197
    | Rche -> Some 25137
    | _    -> None

  let score x = match x with
    | Lpar -> Some 1
    | Lbrk -> Some 2
    | Lbrc -> Some 3
    | Lche -> Some 4
    | _    -> None

  let of_char ch = match ch with
    | '(' -> Some Lpar
    | ')' -> Some Rpar
    | '[' -> Some Lbrk
    | ']' -> Some Rbrk
    | '{' -> Some Lbrc
    | '}' -> Some Rbrc
    | '<' -> Some Lche
    | '>' -> Some Rche
    | _   -> None

  let of_char_exn ch = match of_char ch with
    | Some x -> x
    | None -> raise @@ Invalid_argument (String.of_char ch)

  let which x = match x with
    | Lpar | Lbrk | Lbrc | Lche -> First  x
    | Rpar | Rbrk | Rbrc | Rche -> Second x

  let (=) x y = match (x, y) with
    | Lpar, Lpar -> true
    | Lbrk, Lbrk -> true
    | Lbrc, Lbrc -> true
    | Lche, Lche -> true
    | Rpar, Rpar -> true
    | Rbrk, Rbrk -> true
    | Rbrc, Rbrc -> true
    | Rche, Rche -> true
    |    _,    _ -> false

end

let determine_cost line =
  let _, cost =
    String.to_list line
      |> List.map ~f:Bracket.of_char_exn
      |> List.fold ~init:([], None) ~f:(fun (s, a) x ->
          match a with
          | Some a -> (s, Some a)
          | None   ->
            match Bracket.which x with
            | First x -> (x :: s, a)
            | Second x ->
              let y, z = List.hd s, List.tl s in
              match Option.equal Bracket.(=) (Bracket.should_match x) y with
              | true  -> begin match z with
                  | Some tl -> (tl, a)
                  | None    -> ([], a)
                end
              | false -> ([], Bracket.cost x)) in
  cost

let cost =
  List.map input ~f:(fun line -> determine_cost line)
  |> List.fold ~init:0 ~f:(fun acc c ->
      match c with
      | Some cost -> acc + cost
      | None -> acc)

let () = printf "Cost of syntax errors: %d\n" cost

let score line =
  let points =
    String.to_list line
      |> List.map ~f:Bracket.of_char_exn
        (* drop bad lines, give completion for good lines *)
      |> List.fold ~init:(Some []) ~f:(fun a x ->
          match a with
          | None -> None
          | Some b ->
            match Bracket.which x with
            | First x -> Some (x :: b)
            | Second x ->
              let y, z = List.hd b, List.tl b in
              match Option.equal Bracket.(=) (Bracket.should_match x) y with
              | true  -> begin match z with
                  | Some tl -> Some tl
                  | None    -> Some []
                end
              | false -> None)
      |> (fun y -> match y with
      | None -> None
      | Some l ->
        List.fold l ~init:(Some 0) ~f:(fun a x ->
            let open Option.Monad_infix in
            let s = Bracket.score x in
            s >>= fun y ->
            a >>= fun z ->
            Some (5 * z + y))) in
  points

let middle_score =
  List.map input ~f:(fun line -> score line)
  |> List.fold ~init:[] ~f:(fun a x -> match x with Some y -> y :: a | None -> a)
  |> List.sort ~compare:Int.compare
  |> List.fold ~init:([], 0) ~f:(fun (l, a) x -> x :: l, a + 1)
  |> fun (scores, len) -> List.nth_exn scores (len / 2)

let () = printf "Middle score of completion: %d\n" middle_score
