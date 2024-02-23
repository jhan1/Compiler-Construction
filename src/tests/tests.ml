(** This file should be used to write your tests of your other code. *)

open Batteries;;

open OUnit2;;

open TestUtils;;

open Printf;;

open HatchCompiler.Assembly;;

let instruction_list : instruction list= 
[(AsmAdd(ArgRegister(RAX), (ArgRegister(RSP))));
(AsmAdd(ArgRegister(RAX), (ArgRegister(RSP))));
(AsmMov(ArgMemory(AddrByRegisterOffset(RAX, -28 )), ArgConstant("16") ));
(AsmRet)]

let instruction_list_string = code_of_instruction_list instruction_list;;

(* let () = List.iter (printf "%s") instruction_list_string;;  *)
(* print_endline instruction_list_string *)
let all_tests =
  [
    test_success "test_code/4.bird" "4";
    test_success "test_code/after_4.bird" "5";
    test_success "test_code/before_after_4.bird" "4";
    test_success "test_code/let1.bird" "2";
    test_success "test_code/minus1.bird" "3";
    test_success "test_code/minus3.bird" "2";
    test_success "test_code/plus3.bird" "6";
    test_success "test_code/times2.bird" "15";
    test_success "test_code/arithmetic.bird" "-9";
    test_success "test_code/complex1.bird" "6";
    test_success "test_code/complex2.bird" "2";
    test_compile_failure "test_code/bug1.bird" "Unbound variable z.";
    test_success "test_code/boolean1.bird" "true";
    test_success "test_code/int1.bird" "true";
    test_success "test_code/if1.bird" "true";
    test_success "test_code/if2.bird" "7";
    test_success "test_code/if3.bird" "false";
    test_runtime_failure "test_code/re1.bird" 1;
    (* test_runtime_failure "test_code/use_closure_memory.bird" 7;
    test_runtime_failure "test_code/ME.bird" 7; *)

    (* test_success "test_code/program1.bird" "1";
    test_success "test_code/program2.bird" "true";
    test_success "test_code/program3.bird" "15";
    test_success "test_code/tuple1.bird" "6";
    test_success "test_code/tuple2.bird" "false";
    test_success "test_code/tuple3.bird" "1\n2\n1";
    test_success "test_code/tuple4.bird" "10";
    test_success "test_code/power.bird" "64";
    test_success "test_code/fib.bird" "6765";
    test_success "test_code/tupleSet1.bird" "12";
    test_success "test_code/tail1.bird" "1";
    test_success "test_code/tail2.bird" "(10000003, true)";
    test_success "test_code/tail3.bird" "12";
    test_success "test_code/tail4.bird" "1";
    test_success "test_code/tail5.bird" "6";
    test_success "test_code/tail6.bird" "500000500000";
    test_success "test_code/queens.bird" "10\n4\n40\n92\n352\n724";  *)
    (* test_assembly instruction_list "" *)
  ];;

let suite = "compiler test suite" >::: all_tests;;

run_test_tt_main suite;;
