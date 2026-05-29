let html_escape = Dream.html_escape

(* Render [[wiki links]] to anchors while HTML-escaping everything else.

   We scan manually instead of using [Str.global_substitute] because the gaps
   between matches (the ordinary prose) must be escaped too, and substitute
   only lets us rewrite the matches. For each link, the slug is derived from
   the *raw* label (so "[[A & B]]" slugifies correctly), while the displayed
   label is escaped to neutralise any HTML it contains. *)
let render_wiki_links body =
  let buffer = Buffer.create (String.length body) in
  let append_escaped start len =
    Buffer.add_string buffer (html_escape (String.sub body start len))
  in
  let rec loop pos =
    match Str.search_forward Wiki_link.re body pos with
    | exception Not_found -> append_escaped pos (String.length body - pos)
    | match_start ->
        append_escaped pos (match_start - pos);
        let label = Str.matched_group 1 body in
        let slug = Slug.slugify label in
        Buffer.add_string buffer
          (Printf.sprintf {|<a href="/wiki/%s">%s</a>|} slug (html_escape label));
        loop (Str.match_end ())
  in
  loop 0;
  Buffer.contents buffer

(* Split the body into paragraphs on blank lines (one or more consecutive
   newlines, tolerating trailing spaces), render wiki links within each, and
   wrap each non-empty paragraph in a <p>. *)
let render_body body =
  Str.split (Str.regexp "\n[ \t\r]*\n[ \t\r\n]*") body
  |> List.map (fun paragraph ->
      Printf.sprintf "<p>%s</p>" (render_wiki_links paragraph))
  |> String.concat "\n"

let page ~title ~body =
  Printf.sprintf
    {|<!doctype html>
<html>
<head>
  <title>%s</title>
  <style>
    body { max-width: 760px; margin: 40px auto; font-family: sans-serif; line-height: 1.5; }
    textarea { width: 100%%; min-height: 360px; }
    input { width: 100%%; }
    a.missing { color: #b00; }
  </style>
</head>
<body>
  <main>
    <h1>%s</h1>
    %s
  </main>
</body>
</html>|}
    (html_escape title) (html_escape title) body
