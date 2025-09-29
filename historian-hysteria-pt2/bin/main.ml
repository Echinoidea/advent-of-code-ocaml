(* Parse input file *)
let read_lines file =
  let content = In_channel.with_open_bin file In_channel.input_all in
  let lines = String.split_on_char '\n' content in
  lines

(* Parse the line to extract a tuple of int int *)
let get_pair line =
  let split = String.split_on_char ' ' line in
  let first = List.nth split 0 |> int_of_string in
  let last = List.nth (List.rev split) 0 |> int_of_string in
  (first, last)

(* Pipe operator |> ... x |> f == f (x)  *)
let tuple_list =
  read_lines
    "/home/gabriel/code/ocaml/advent-of-code/historian-hysteria/input.txt"
  |> List.filter (fun line -> String.trim line <> "")
  |> List.map get_pair

(* Unzip the list of pairs into a pair of lists, sort them, return tuple of lists *)
let sort_list l =
  let split = List.split l in
  let l1 = List.sort compare (fst split) in
  let l2 = List.sort compare (snd split) in
  (l1, l2)

(* Given a value v and a list l, count the number of occurrences of v in l *)
let count_occurrences v l = List.filter (fun x -> x = v) l |> List.length

(* Accepts a tuple of lists (int list l1, int list l2). *)
(*  Iterates through l1 and count the number of occurences v in l2 ( = n). *)
(*  Then, set l1[i] to li[i] * n. *)
(*  Returns a new tuple (l1, l2) where l1 has new calculated values *)
let occurrences_scores list_pair_int =
  let new_l1 =
    List.map
      (fun elem -> elem * count_occurrences elem (snd list_pair_int))
      (fst list_pair_int)
  in
  (new_l1, snd list_pair_int)

let calc_similarity_score list_pair_int =
  List.fold_left ( + ) 0 (fst list_pair_int)

let () =
  tuple_list |> sort_list |> occurrences_scores |> calc_similarity_score
  |> Printf.printf "%d\n"
