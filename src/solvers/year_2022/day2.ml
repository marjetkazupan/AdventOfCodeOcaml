open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let shape_to_score = function
    | "X" | "A" -> 1
    | "Y" | "B" -> 2
    | "Z" | "C" -> 3
    | _ -> failwith "shape_to_score narobe"

  let naloga1 data =
    let lines = List.lines data in
    let shape_to_win = function
      | "A", "X" | "B", "Y" | "C", "Z" -> 3
      | "A", "Y" | "B", "Z" | "C", "X" -> 6
      | "A", "Z" | "B", "X" | "C", "Y" -> 0
      | _ -> failwith "shape_to_win narobe"
    in
    let rec naloga1_aux acc = function
      | [] -> string_of_int acc
      | x :: xs ->
          naloga1_aux
            ( acc
            + shape_to_score (String.sub x 2 1)
            + shape_to_win (String.sub x 0 1, String.sub x 2 1) )
            xs
    in
    naloga1_aux 0 lines

  let naloga2 data _part1 =
    let shape_to_win = function
      | "A" -> "B"
      | "B" -> "C"
      | "C" -> "A"
      | _ -> failwith "narobe"
    in
    let shape x = function
      | "X" -> shape_to_win (shape_to_win x)
      | "Y" -> x
      | "Z" -> shape_to_win x
      | _ -> failwith "narobe"
    in
    let ending_score = function
      | "X" -> 0
      | "Y" -> 3
      | "Z" -> 6
      | _ -> failwith "narobe"
    in
    let rec naloga2_aux acc = function
      | [] -> string_of_int acc
      | x :: xs ->
          naloga2_aux
            ( acc
            + shape_to_score (shape (String.sub x 0 1) (String.sub x 2 1))
            + ending_score (String.sub x 2 1) )
            xs
    in
    let lines = List.lines data in
    naloga2_aux 0 lines
end
