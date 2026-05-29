let html_escape = Dream.html_escape

let render_wiki_links body =
  Str.global_substitute Wiki_link.re
    (fun s ->
      let label = Str.matched_group 1 s in
      let slug = Slug.slugify label in
      Printf.sprintf {|<a href="/wiki/%s">%s</a>|} slug (html_escape label))
    body

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
