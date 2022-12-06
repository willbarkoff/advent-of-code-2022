let explode s = List.init (String.length s) (String.get s)
let line = read_line () |> explode

let rec find_four_diff a b c i acc =
  match acc with
  | h :: t ->
      if h <> a && h <> b && h <> c && a <> b && a <> c && b <> c then i
      else find_four_diff b c h (i + 1) t
  | _ -> failwith "BAD!"

let _ =
  line
  |> (function
       | h1 :: h2 :: h3 :: t -> find_four_diff h1 h2 h3 4 t
       | _ -> failwith "BAD2")
  |> print_int
