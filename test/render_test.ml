let check_render input expected =
  let actual = Microwiki.Render.render_body input in
  Alcotest.(check string) input expected actual

let test_render_body_cases () =
  List.iter
    (fun (input, expected) -> check_render input expected)
    [
      (* prose is HTML-escaped *)
      ("a <b> & \"c\"", {|<p>a &lt;b&gt; &amp; &quot;c&quot;</p>|});
      (* blank lines split paragraphs; single newlines do not *)
      ("one\n\ntwo", "<p>one</p>\n<p>two</p>");
      ("line one\nline two", "<p>line one\nline two</p>");
      (* multiple blank lines collapse into a single split *)
      ("a\n\n\n\nb", "<p>a</p>\n<p>b</p>");
      (* basic wiki link *)
      ("see [[Home]]", {|<p>see <a href="/wiki/home">Home</a></p>|});
      (* slug from the raw label, display label escaped *)
      ("[[A & B]]", {|<p><a href="/wiki/a-b">A &amp; B</a></p>|});
      (* a link label that itself contains markup cannot inject tags *)
      ("[[<b>x</b>]]", {|<p><a href="/wiki/b-x-b">&lt;b&gt;x&lt;/b&gt;</a></p>|});
    ]

let test_missing_link_styling () =
  (* Only "home" exists; the link to a missing page gets class="missing". *)
  let exists slug = slug = "home" in
  Alcotest.(check string)
    "missing link styled"
    {|<p><a href="/wiki/home">Home</a> and <a class="missing" href="/wiki/ghost">Ghost</a></p>|}
    (Microwiki.Render.render_body ~exists "[[Home]] and [[Ghost]]")

let () =
  Alcotest.run "render"
    [
      ( "render_body",
        [
          Alcotest.test_case "render_body cases" `Quick test_render_body_cases;
          Alcotest.test_case "missing link styling" `Quick
            test_missing_link_styling;
        ] );
    ]
