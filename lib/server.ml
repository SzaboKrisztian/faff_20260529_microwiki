let layout title body = Dream.html (Render.page ~title ~body)

(* Run a database action against the pool and unwrap the result. On a Caqti
   error we raise, which Dream's top-level handler turns into a 500 response.
   Handlers below can therefore work with plain values instead of [result]s. *)
let query pool f =
  match%lwt Db.use pool f with
  | Ok value -> Lwt.return value
  | Error err -> failwith (Caqti_error.show err)

let home_page pool _request =
  let%lwt pages = query pool Db.list_all in
  let items =
    List.map
      (fun (p : Page.t) ->
        Printf.sprintf {|<li><a href="/wiki/%s">%s</a></li>|}
          (Dream.html_escape p.slug)
          (Dream.html_escape p.title))
      pages
  in
  let list_html =
    match items with
    | [] -> "<p>No pages yet.</p>"
    | _ -> "<ul>" ^ String.concat "" items ^ "</ul>"
  in
  (* Create-by-name form. This is a GET that only redirects (no mutation), so
     it needs no CSRF token; the actual write happens on the edit form's POST. *)
  let create_form =
    {|<form method="get" action="/create">
<p><input name="title" placeholder="New page title"></p>
<p><button>Create page</button></p>
</form>|}
  in
  layout "Microwiki" (list_html ^ create_form)

(* Turn a typed page name into a slug and send the user to its edit form,
   carrying the original title along so a new page is pre-filled with it. *)
let create_page _pool request =
  let title = Option.value ~default:"" (Dream.query request "title") in
  match Slug.slugify title with
  | "" -> Dream.redirect request "/"
  | slug ->
      let target =
        Uri.make
          ~path:("/wiki/" ^ slug ^ "/edit")
          ~query:[ ("title", [ title ]) ]
          ()
      in
      Dream.redirect request (Uri.to_string target)

let backlinks_section sources =
  match sources with
  | [] -> ""
  | _ ->
      let items =
        List.map
          (fun (from_slug, from_title) ->
            Printf.sprintf {|<li><a href="/wiki/%s">%s</a></li>|}
              (Dream.html_escape from_slug)
              (Dream.html_escape from_title))
          sources
      in
      Printf.sprintf {|<hr><h2>Linked from</h2><ul>%s</ul>|}
        (String.concat "" items)

let view_page pool request =
  let slug = Dream.param request "slug" in
  let%lwt found = query pool (fun conn -> Db.find_by_slug conn slug) in
  let%lwt sources = query pool (fun conn -> Db.backlinks conn slug) in
  let backlinks = backlinks_section sources in
  match found with
  | Some page ->
      let body_html = Render.render_body page.Page.body in
      layout page.Page.title
        (Printf.sprintf {|%s<p><a href="/wiki/%s/edit">Edit</a></p>%s|}
           body_html (Dream.html_escape slug) backlinks)
  | None ->
      layout slug
        (Printf.sprintf
           {|<p>This page does not exist yet.</p>
<p><a class="missing" href="/wiki/%s/edit">Create it</a></p>%s|}
           (Dream.html_escape slug) backlinks)

let edit_page pool request =
  let slug = Dream.param request "slug" in
  let%lwt found = query pool (fun conn -> Db.find_by_slug conn slug) in
  let title, body =
    match found with
    | Some page -> (page.Page.title, page.Page.body)
    | None ->
        (* New page: prefill the title from ?title= (set by the create form),
           falling back to the slug itself. *)
        let suggested =
          Option.value ~default:slug (Dream.query request "title")
        in
        (suggested, "")
  in
  layout ("Edit " ^ slug)
    (Printf.sprintf
       {|<form method="post" action="/wiki/%s">
%s
<p><input name="title" value="%s"></p>
<p><textarea name="body">%s</textarea></p>
<p><button>Save</button></p>
</form>|}
       (Dream.html_escape slug)
       (* CSRF token tied to the session; Dream.form validates it on POST. *)
       (Dream.csrf_tag request)
       (Dream.html_escape title) (Dream.html_escape body))

let save_page pool request =
  let url_slug = Dream.param request "slug" in
  let%lwt form = Dream.form request in
  match form with
  | `Ok fields ->
      let field name default =
        match List.assoc_opt name fields with
        | Some value -> value
        | None -> default
      in
      let title = field "title" url_slug in
      let body = field "body" "" in
      let%lwt existing =
        query pool (fun conn -> Db.find_by_slug conn url_slug)
      in
      (* Slugs are stable: an existing page keeps its slug even if the title
         changes. Only new pages derive their slug from the title (falling back
         to the URL slug when the title has no slug-worthy characters). *)
      let slug =
        match existing with
        | Some _ -> url_slug
        | None -> ( match Slug.slugify title with "" -> url_slug | s -> s)
      in
      let%lwt () = query pool (fun conn -> Db.save conn ~slug ~title ~body) in
      Dream.redirect request ("/wiki/" ^ slug)
  | _ -> Dream.respond ~status:`Bad_Request "Bad form"

let run () =
  match Db.connect "sqlite3:wiki.db" with
  | Error err -> failwith ("Could not open database: " ^ Caqti_error.show err)
  | Ok pool ->
      (* Create the schema once at startup before serving any requests. *)
      (match Lwt_main.run (Db.use pool Db.init) with
      | Ok () -> ()
      | Error err ->
          failwith ("Could not initialise database: " ^ Caqti_error.show err));
      Dream.run @@ Dream.logger @@ Dream.memory_sessions
      @@ Dream.router
           [
             Dream.get "/" (home_page pool);
             Dream.get "/create" (create_page pool);
             Dream.get "/wiki/:slug" (view_page pool);
             Dream.get "/wiki/:slug/edit" (edit_page pool);
             Dream.post "/wiki/:slug" (save_page pool);
           ]
