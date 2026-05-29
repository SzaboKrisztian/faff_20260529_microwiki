let layout title body = Dream.html (Render.page ~title ~body)

let run () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             layout "Microwiki" {|<p><a href="/wiki/home">Home</a></p>|});
         Dream.get "/wiki/:slug" (fun request ->
             let slug = Dream.param request "slug" in
             layout slug
               (Printf.sprintf
                  {|<p>This will show page: <code>%s</code></p>
<p><a href="/wiki/%s/edit">Edit</a></p>|}
                  (Dream.html_escape slug) (Dream.html_escape slug)));
         Dream.get "/wiki/:slug/edit" (fun request ->
             let slug = Dream.param request "slug" in
             layout ("Edit " ^ slug)
               (Printf.sprintf
                  {|<form method="post" action="/wiki/%s">
<p><input name="title" value="%s"></p>
<p><textarea name="body"></textarea></p>
<p><button>Save</button></p>
</form>|}
                  (Dream.html_escape slug) (Dream.html_escape slug)));
         Dream.post "/wiki/:slug" (fun request ->
             let slug = Dream.param request "slug" in
             let%lwt form = Dream.form request in
             match form with
             | `Ok fields ->
                 let title =
                   match List.assoc_opt "title" fields with
                   | Some value -> value
                   | None -> slug
                 in
                 let body =
                   match List.assoc_opt "body" fields with
                   | Some value -> value
                   | None -> ""
                 in
                 let rendered = Render.render_wiki_links body in
                 layout title rendered
             | _ -> Dream.respond ~status:`Bad_Request "Bad form");
       ]
