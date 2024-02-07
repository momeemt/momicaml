let rec substitute tvar t te =
  match te with
  | [] -> []
  | (x, t2) :: te2 ->
      let t3 = if t2 = tvar then t else t2 in
      (x, t3) :: substitute tvar t te2

let test_substitute () =
  let rec run i tvar t te =
    if i = 0 then ()
    else (
      substitute tvar t te |> ignore;
      run (i - 1) tvar t te)
  in
  run 100000000 "a" "b" [ ("x", "a"); ("y", "b"); ("z", "c") ];
  Alcotest.(check bool) "test_substitute" true true

let () =
  Alcotest.run "Momicaml.substitute-1"
    [
      ( "test_substitute",
        [ Alcotest.test_case "test_substitute" `Quick test_substitute ] );
    ]
