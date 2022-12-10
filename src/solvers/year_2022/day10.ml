open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let naloga1 data =
    let lines = List.map (String.split_on_char ' ') (List.lines data) in
    let rec naloga1_aux value acc = function
      | [] -> acc
      | x :: xs -> (
          match x with
          | "noop" :: _ -> naloga1_aux value (Array.append acc [| value |]) xs
          | "addx" :: istr :: _ ->
              naloga1_aux
                (value + int_of_string istr)
                (Array.append acc [| value; value |])
                xs
          | _ -> failwith "narobe" )
    in
    let sez = Array.mapi (fun i a -> i * a) (naloga1_aux 1 [| 0 |] lines) in
    let num =
      sez.(20) + sez.(60) + sez.(100) + sez.(140) + sez.(180) + sez.(220)
    in
    string_of_int num

  let naloga2 data _part1 =
    let lines = List.map (String.split_on_char ' ') (List.lines data) in
    let rec naloga2_aux cycle value = function
      | [] -> ""
      | x :: xs -> (
          match x with
          | "noop" :: _ ->
              let () =
                if abs ((cycle mod 40) - value) < 2 then print_string "#"
                else print_string "."
              and () = if cycle mod 40 = 39 then print_newline () else () in
              naloga2_aux ((cycle mod 40) + 1) value xs
          | "addx" :: istr :: _ ->
              let () =
                if abs ((cycle mod 40) - value) < 2 then print_string "#"
                else print_string "."
              and () = if cycle mod 40 = 39 then print_newline () else () in
              let () =
                if abs (((cycle + 1) mod 40) - value) < 2 then print_string "#"
                else print_string "."
              and () =
                if (cycle + 1) mod 40 = 39 then print_newline () else ()
              in
              naloga2_aux ((cycle mod 40) + 2) (value + int_of_string istr) xs
          | _ -> failwith "narobe" )
    in
    naloga2_aux 0 1 lines
end
