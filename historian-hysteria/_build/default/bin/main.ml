let read_lines file =
  let content = In_channel.with_open_bin file In_channel.input_all in
  let lines = String.split_on_char '\n' content in
  lines

let get_pair line =
  let split = String.split_on_char ' ' line in
  let first = List.nth split 0 in
  let last = List.nth (List.rev split) 0 in
  (first, last)

(* Pipe operator |> ... x |> f == f (x)  *)
let tuple_list =
  read_lines "/home/gabriel/code/ocaml/historian-hysteria/input.txt"
  |> List.filter (fun line -> String.trim line <> "")
  |> List.map get_pair

let sort_list l =
  let split = List.split l in
  let l1 = List.sort compare (fst split) in
  let l2 = List.sort compare (snd split) in
  List.combine l1 l2

let calc_pair_distance pair =
  let first_int = int_of_string (String.trim (fst pair)) in
  let second_int = int_of_string (String.trim (snd pair)) in
  abs (first_int - second_int)

let _distances pairs = List.map (fun pair -> calc_pair_distance pair) pairs

let calc_total_distance pairs =
  let distances = List.map (fun pair -> calc_pair_distance pair) pairs in
  List.fold_left ( + ) 0 distances

let _print_list l = List.iter (fun e -> Printf.printf "%d " e) l

let _print_pairs_list () =
  List.iter
    (fun (first, second) -> Printf.printf "%s  %s\n" first second)
    tuple_list

let () = Printf.printf "%d\n" (calc_total_distance (sort_list tuple_list))
