open Momicaml.Cam
open Result

let test_cam1 () =
  let cam =
    [
      CAM.CAM_Closure
        [
          CAM.CAM_Ldi 1;
          CAM.CAM_Access 0;
          CAM.CAM_Eq;
          CAM.CAM_Test
            ( [ CAM.CAM_Ldi 1 ],
              [
                CAM.CAM_Ldi (-1);
                CAM.CAM_Access 0;
                CAM.CAM_Add;
                CAM.CAM_Access 1;
                CAM.CAM_Apply;
                CAM.CAM_Access 0;
                CAM.CAM_Add;
              ] );
          CAM.CAM_Return;
        ];
      CAM.CAM_Let;
      CAM.CAM_Ldi 10;
      CAM.CAM_Access 0;
      CAM.CAM_Apply;
      CAM.CAM_EndLet;
    ]
  in
  let stack_res = CAM.eval (ok cam) [] [] in
  let stack = match stack_res with Ok s -> s | Error _ -> [] in
  let res = List.hd stack in
  match res with
  | CAM.CAM_IntCal i -> Alcotest.(check int) "test_cam1" 55 i
  | _ -> Alcotest.fail "test_cam1"

let () =
  Alcotest.run "Momicaml"
    [ ("test_cam1", [ Alcotest.test_case "test_cam1" `Quick test_cam1 ]) ]
