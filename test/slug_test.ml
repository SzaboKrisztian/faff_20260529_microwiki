let check_slug input expected =
  let actual = Microwiki.Slug.slugify input in
  Alcotest.(check string) input expected actual

let test_slugify_cases () =
  List.iter
    (fun (input, expected) -> check_slug input expected)
    [
      ("Hello World", "hello-world");
      ("OCaml is great!", "ocaml-is-great");
      ("  Leading and trailing spaces  ", "leading-and-trailing-spaces");
      ("Multiple   spaces", "multiple-spaces");
      ("Special_characters!@#$%^&*()", "special-characters");
      ("MixedCASE123", "mixedcase123");
      ("already-slugified", "already-slugified");
      ("", "");
      ("!!!", "");
      ("Café Society", "caf-society");
    ]

let test_output_contains_only_slug_chars () =
  List.iter
    (fun input ->
      let output = Microwiki.Slug.slugify input in
      String.iter
        (fun c ->
          let ok =
            match c with 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false
          in
          Alcotest.(check bool)
            ("bad char in output for input: " ^ input)
            true ok)
        output)
    [
      "Hello World!"; "OCaml 5.2.1"; "Café Society"; "what???"; "hello---world";
    ]

let () =
  Alcotest.run "slug"
    [
      ( "slugify",
        [
          Alcotest.test_case "slugify cases" `Quick test_slugify_cases;
          Alcotest.test_case "output contains only slug chars" `Quick
            test_output_contains_only_slug_chars;
        ] );
    ]
