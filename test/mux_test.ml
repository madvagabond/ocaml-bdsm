open Codec_tests 
open OUnit2
       
  

  



let run_async f ctx =
  Lwt_main.run (f ctx)
              
               
                  
let suite =
  "Codec Suite" >:::
    [
      "rmsg codec test" >:: rmsg_codec;
      "header codec test" >:: header_codec;
      "tmsg codec" >:: tmsg_codec;
      "reader test" >:: (run_async reader_test);
      "client_server test" >:: (run_async Hw_server.test)


    ]

      
let () =
  Lwt.async (fun () -> Hw_server.listener () );
  run_test_tt_main suite
    


