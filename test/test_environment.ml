open Momicaml.Environment

let test_lookup () =
  Alcotest.(check int) "lookup" 1 (Environment.lookup "x" (Environment.ext (Environment.emptyEnv()) "x" 1))

let () =
  Alcotest.run "Momicaml.lib" [
    ("Environment.liikup", [
        Alcotest.test_case "lookup" `Quick test_lookup;
      ]);
  ]

