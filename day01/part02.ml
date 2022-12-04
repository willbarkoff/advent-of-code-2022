let elf_data =
  let rec read_elf_data data_so_far current_item =
    try
      let line = read_line () in
      if line = "" then read_elf_data (current_item :: data_so_far) []
      else read_elf_data data_so_far (int_of_string line :: current_item)
    with End_of_file -> current_item :: data_so_far
  in
  read_elf_data [] []

let _ =
  elf_data
  |> List.map (List.fold_left ( + ) 0)
  |> List.fold_left
       (fun (t0, t1, t2) n ->
         if n > t0 then
           if n > t1 then if n > t2 then (t1, t2, n) else (t1, n, t2)
           else (n, t1, t2)
         else (t0, t1, t2))
       (0, 0, 0)
  |> (fun (a, b, c) -> a + b + c)
  |> print_int;
  print_newline ()
