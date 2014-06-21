open OUnit


module CH = Consistent_hash.Make (Digest)


let v1v2 =
  CH.add "key1" "value1" (CH.add "key2" "value2" (CH.make ~interleave_count:1 ()))


let count h =
  let i = ref 0 in
  CH.iter (fun _ _ -> incr i) h;
  !i


(*
 * 943901380 1
 * 977592659 value2
 * 1046517191 value1
 * 1432200731 value1 (key1-0)
 * 2216742351 0
 * 2710406963 value2
 * 4027086333 value2
 * 4095887727 18
 * 4288362609 value1
 * 4290808872 769
 *)


let suite = "consistent hash suite" >::: [
  "test_empty" >:: (fun () ->
    let h = CH.make () in
    assert_equal (count h) 0);
  "test_one_key_default" >:: (fun () ->
    let h = CH.add "key1" "value1" (CH.make ()) in
    assert_equal (count h) 120);
  "test_one_key_smaller_interleave" >:: (fun () ->
    let h = CH.add "key1" "value1" (CH.make ~interleave_count:1 ()) in
    assert_equal (count h) 3);
  "test_one_key_slightly_larger_interleave" >:: (fun () ->
    let h = CH.add "key1" "value1" (CH.make ~interleave_count:42 ()) in
    assert_equal (count h) 126);
  "test_greater_than_all" >:: (fun () ->
    assert_equal (CH.find "769" v1v2) "value2");
  "test_less_than_all" >:: (fun () ->
    assert_equal (CH.find "1" v1v2) "value2");
  "test_exactly_equal" >:: (fun () ->
    assert_equal (CH.find "key1-0" v1v2) "value1");
  "test_between_one_and_two" >:: (fun () ->
    assert_equal (CH.find "0" v1v2) "value2");
  "test_between_two_and_one" >:: (fun () ->
    assert_equal (CH.find "18" v1v2) "value1");
]


let _ =
  run_test_tt_main suite
