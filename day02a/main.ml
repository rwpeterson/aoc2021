open Core
open Stdio

let horiz, depth = In_channel.with_file "day02a/input.txt" ~f:(fun file ->
    In_channel.fold_lines file ~init:(0, 0) ~f:(fun (h, d) line ->
        match String.split ~on:' ' line with
        | "forward" :: x :: [] -> h + Int.of_string x, d
        | "up" :: y :: [] -> h, d - Int.of_string y
        | "down" :: y :: [] -> h, d + Int.of_string y
        | _ -> h, d))

let () =
  printf "Horizontal position %d * depth %d = %d\n" horiz depth (horiz * depth)
