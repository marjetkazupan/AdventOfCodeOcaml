open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let to_array lst =
    let a = Array.of_list lst in
    Array.map (fun x -> (x, false)) a

  let string_to_intarray str =
    Array.map
      (fun x -> (int_of_char x, false))
      (Array.init (String.length str) (String.get str))

  let string_to_intarray2 str =
    Array.map
      (fun x -> (int_of_char x, 0))
      (Array.init (String.length str) (String.get str))

  let check_row grid index =
    let rec check_aux acc = function
      | a when a = Array.length grid.(index) -> ()
      | c ->
          let x, _ = grid.(index).(c) in
          if x > acc then
            let () = grid.(index).(c) <- (x, true) in
            check_aux x (c + 1)
          else check_aux acc (c + 1)
    in
    check_aux 0 0

  let check_column grid index =
    let rec check_aux acc = function
      | a when a = Array.length grid -> ()
      | c ->
          let x, _ = grid.(c).(index) in
          if x > acc then
            let () = grid.(c).(index) <- (x, true) in
            check_aux x (c + 1)
          else check_aux acc (c + 1)
    in
    check_aux 0 0

  let reverse_grid grid =
    Array.of_list
      (List.rev
         (Array.to_list
            (Array.map Array.of_list
               (Array.map List.rev (Array.map Array.to_list grid)))))

  let count grid i j =
    let x, _ = grid.(i).(j) in
    let maxx, maxy = (Array.length grid.(0), Array.length grid) in
    let rec count_aux acc (iii, jjj) (dx, dy) =
      let ii, jj = (iii + dx, jjj + dy) in
      if ii = maxx || jj = maxy || ii < 0 || jj < 0 then acc
      else
        let d, _ = grid.(ii).(jj) in
        if x <= d then acc + 1 else count_aux (acc + 1) (ii, jj) (dx, dy)
    in
    let score =
      List.fold_left ( * ) 1
        (List.map (count_aux 0 (i, j)) [ (1, 0); (0, 1); (-1, 0); (0, -1) ])
    in
    let () = grid.(i).(j) <- (x, score) in
    ()

  let naloga1 data =
    let lines = List.lines data in
    let grid = Array.map string_to_intarray (Array.of_list lines) in
    let rows_num = Array.length grid in
    let column_num = Array.length grid.(0) in
    let a = List.init rows_num (check_row grid) in
    let a = List.init column_num (check_column grid) in
    let grid = reverse_grid grid in
    let a = List.init rows_num (check_row grid) in
    let a = List.init column_num (check_column grid) in
    let num =
      Array.fold_left ( + ) 0
        (Array.map List.length
           (Array.map
              (List.filter (fun (_, y) -> y))
              (Array.map Array.to_list grid)))
    in
    string_of_int num

  let naloga2 data _part1 =
    let lines = List.lines data in
    let grid = Array.map string_to_intarray2 (Array.of_list lines) in
    let rows_num = Array.length grid in
    let column_num = Array.length grid.(0) in
    let a =
      List.flatten
        (List.map (List.init rows_num) (List.init column_num (count grid)))
    in
    let num =
      Array.fold_left max 0
        (Array.map (Array.fold_left max 0)
           (Array.map (Array.map (fun (_, y) -> y)) grid))
    in
    string_of_int num
end
