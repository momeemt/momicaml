let substitute tvar t te =
  List.map (fun (x, t2) -> if t2 = tvar then (x, t) else (x, t2)) te

let test_substitute () =
  let rec run i tvar t te =
    if i == 0 then ()
    else (
      substitute tvar t te |> ignore;
      run (i - 1) tvar t te)
  in
  run 100000000 "a" "b" [ ("x", "a"); ("y", "b"); ("z", "c") ];
  Alcotest.(check bool) "test_substitute" true true

let () =
  Alcotest.run "Momicaml.substitute2"
    [
      ( "test_substitute",
        [ Alcotest.test_case "test_substitute" `Quick test_substitute ] );
    ]
