let explode s = List.init (String.length s) (String.get s)
let line = read_line () |> explode

let take n lst =
  let rec aux n acc = function
    | h :: tl as lst -> if n = 0 then (acc, lst) else aux (n - 1) (h :: acc) tl
    | _ -> failwith "TOO SHORT"
  in
  let h, t = aux n [] lst in
  (List.rev h, t)

let rec find_diff lst i acc =
  match acc with
  | h :: t ->
      (Printf.printf "%c\n") h;
      if List.sort_uniq compare lst |> List.length = List.length lst then i
      else find_diff (List.tl lst @ [ h ]) (i + 1) t
  | _ -> failwith "BAD!"

let beg, en = take 14 line
let _ = find_diff beg 14 en |> print_int
