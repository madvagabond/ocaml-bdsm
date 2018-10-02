
       
let () =
  Alcotest.run "Testing Mux" [
    "Headers", Header_suite.tests;
    "Frame", Frame_suite.tests;
    "Messages", Messages.suite
  ]
