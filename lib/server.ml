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
  layout "Microwiki"
    (list_html ^ {|<p><a href="/wiki/home/edit">Create the home page</a></p>|})

let view_page pool request =
  let slug = Dream.param request "slug" in
  let%lwt found = query pool (fun conn -> Db.find_by_slug conn slug) in
  match found with
  | Some page ->
      let body_html = Render.render_body page.Page.body in
      layout page.Page.title
        (Printf.sprintf {|%s<p><a href="/wiki/%s/edit">Edit</a></p>|} body_html
           (Dream.html_escape slug))
  | None ->
      layout slug
        (Printf.sprintf
           {|<p>This page does not exist yet.</p>
<p><a class="missing" href="/wiki/%s/edit">Create it</a></p>|}
           (Dream.html_escape slug))

let edit_page pool request =
  let slug = Dream.param request "slug" in
  let%lwt found = query pool (fun conn -> Db.find_by_slug conn slug) in
  let title, body =
    match found with
    | Some page -> (page.Page.title, page.Page.body)
    | None -> (slug, "")
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
  let slug = Dream.param request "slug" in
  let%lwt form = Dream.form request in
  match form with
  | `Ok fields ->
      let field name default =
        match List.assoc_opt name fields with
        | Some value -> value
        | None -> default
      in
      let title = field "title" slug in
      let body = field "body" "" in
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
             Dream.get "/wiki/:slug" (view_page pool);
             Dream.get "/wiki/:slug/edit" (edit_page pool);
             Dream.post "/wiki/:slug" (save_page pool);
           ]
