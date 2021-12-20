open Base
open Stdio

let rec read_and_acc_win acc lasts =
  let line = In_channel.input_line In_channel.stdin in
  match line with
  | None -> acc
  | Some xstr -> let x = Int.of_string xstr in
    match lasts with
    | (None,   None,   None)   -> read_and_acc_win acc (Some x, None,   None)
    | (Some a, None,   None)   -> read_and_acc_win acc (Some x, Some a, None)
    | (Some a, Some b, None)   -> read_and_acc_win acc (Some x, Some a, Some b)
    | (Some a, Some b, Some c) -> begin match x + a + b > a + b + c with
      | true ->  read_and_acc_win (acc + 1) (Some x, Some a, Some b)
      | false -> read_and_acc_win acc       (Some x, Some a, Some b)
    end
    | _ -> read_and_acc_win acc (None, None, None)

let () =
  printf "Total 3-window increases: %d\n" (read_and_acc_win 0 (None, None, None))
