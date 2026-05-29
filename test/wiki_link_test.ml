let check_extract input expected =
  let actual = Microwiki.Wiki_link.extract input in
  Alcotest.(check (list Microwiki.Wiki_link.testable) input expected actual)

let test_extract_cases () =
  List.iter
    (fun (input, expected) -> check_extract input expected)
    [
      ("Hello [[World]]", [ { label = "World"; slug = "world" } ]);
      ( "Testing [[Multiple links]] [[with spaces]]",
        [
          { label = "Multiple links"; slug = "multiple-links" };
          { label = "with spaces"; slug = "with-spaces" };
        ] );
      ("No links", []);
    ]

let () =
  Alcotest.run "wiki_link"
    [
      ( "extract",
        [ Alcotest.test_case "extract cases" `Quick test_extract_cases ] );
    ]
