open Base
open Stdio

let rec read_and_accumulate acc last =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> acc
  | Some x -> let cur = Int.of_string x in
    match last with
    | None -> read_and_accumulate acc (Some cur)
    | Some y -> match cur > y with
      | true -> read_and_accumulate (acc + 1) (Some cur)
      | false -> read_and_accumulate acc (Some cur)

let () =
  printf "Total relative increases: %d\n" (read_and_accumulate 0 None)
