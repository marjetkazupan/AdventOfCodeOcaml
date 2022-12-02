open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let naloga1 data =
    let lines = List.lines data in
    let shape_to_score = function
      | "X" -> 1
      | "Y" -> 2
      | "Z" -> 3
      | _ -> failwith "shape_to_score narobe"
    in
    let shape_to_win = function
      | "A", "X" -> 3
      | "B", "Y" -> 3
      | "C", "Z" -> 3
      | "A", "Y" -> 6
      | "B", "Z" -> 6
      | "C", "X" -> 6
      | "A", "Z" -> 0
      | "B", "X" -> 0
      | "C", "Y" -> 0
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

  let naloga2 data odgovor_prve_naloge =
    let shape_to_win = function
      | "A" -> "B"
      | "B" -> "C"
      | "C" -> "A"
      | _ -> failwith "narobe"
    in
    let shape_to_lose = function
      | "A" -> "C"
      | "B" -> "A"
      | "C" -> "B"
      | _ -> failwith "narobe"
    in
    let shape x = function
      | "X" -> shape_to_lose x
      | "Y" -> x
      | "Z" -> shape_to_win x
      | _ -> failwith "narobe"
    in
    let score = function
      | "A" -> 1
      | "B" -> 2
      | "C" -> 3
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
            + score (shape (String.sub x 0 1) (String.sub x 2 1))
            + ending_score (String.sub x 2 1) )
            xs
    in
    let lines = List.lines data in
    naloga2_aux 0 lines
end
