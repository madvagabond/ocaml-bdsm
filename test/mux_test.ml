

open Client_server
    
let () =
  Alcotest.run "Testing Mux" [
    "Headers", Header_suite.tests;
    "Frame", Frame_suite.tests;
    "Messages", Messages.suite;
    
    "Client Server", Client_server.tests
  ]
