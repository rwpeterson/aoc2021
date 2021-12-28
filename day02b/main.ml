open Core
open Stdio

let horiz, depth, aim = In_channel.with_file "day02b/input.txt" ~f:(fun file ->
    In_channel.fold_lines file ~init:(0, 0, 0) ~f:(fun (h, d, a) line ->
        match String.split ~on:' ' line with
        | "forward" :: x :: [] ->
          h + Int.of_string x, d + Int.of_string x * a, a
        | "up" :: y :: [] ->
          h, d, a - Int.of_string y
        | "down" :: y :: [] ->
          h, d, a + Int.of_string y
        | _ -> h, d, a))

let () =
  printf "Horizontal position %d * depth %d = %d\n" horiz depth (horiz * depth)
