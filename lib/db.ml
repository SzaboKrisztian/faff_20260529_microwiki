(* A thin persistence layer for wiki pages, backed by SQLite via Caqti.

   The design splits cleanly in two:
   - [Q] holds the typed SQL statements (pure values, compiled once).
   - The functions below run those statements against a connection, and a
     small pool wrapper lets callers run a unit of work without juggling
     connections by hand. *)

(* A Caqti type describing how a [Page.t] maps to/from a row of six columns.
   We only ever read full rows here, so [decode] is the interesting half;
   [encode] is provided for completeness (e.g. if this type is reused as a
   query parameter). The DB columns are non-nullable, so we wrap the values
   the record stores as options back into [Some] on decode. *)
let page_type =
  let open Caqti_type in
  let encode (p : Page.t) =
    Ok
      ( Option.value ~default:0 p.id,
        p.slug,
        p.title,
        p.body,
        Option.value ~default:"" p.created_at,
        Option.value ~default:"" p.updated_at )
  in
  let decode (id, slug, title, body, created_at, updated_at) =
    Ok (Page.make ~id ~slug ~title ~body ~created_at ~updated_at ())
  in
  custom ~encode ~decode (t6 int string string string string string)

module Q = struct
  open Caqti_request.Infix

  let create_table =
    (Caqti_type.unit ->. Caqti_type.unit)
      {sql| CREATE TABLE IF NOT EXISTS pages (
              id         INTEGER PRIMARY KEY AUTOINCREMENT,
              slug       TEXT NOT NULL UNIQUE,
              title      TEXT NOT NULL,
              body       TEXT NOT NULL,
              created_at TEXT NOT NULL DEFAULT (datetime('now')),
              updated_at TEXT NOT NULL DEFAULT (datetime('now'))
            ) |sql}

  (* Insert or, if the slug already exists, update in place. SQLite's
     "upsert" via ON CONFLICT keeps created_at but refreshes updated_at. *)
  let upsert =
    (Caqti_type.(t3 string string string) ->. Caqti_type.unit)
      {sql| INSERT INTO pages (slug, title, body)
            VALUES (?, ?, ?)
            ON CONFLICT(slug) DO UPDATE SET
              title = excluded.title,
              body = excluded.body,
              updated_at = datetime('now') |sql}

  let find_by_slug =
    (Caqti_type.string ->? page_type)
      {sql| SELECT id, slug, title, body, created_at, updated_at
            FROM pages WHERE slug = ? |sql}

  let list_all =
    (Caqti_type.unit ->* page_type)
      {sql| SELECT id, slug, title, body, created_at, updated_at
            FROM pages ORDER BY title |sql}

  (* One row per [[wiki link]] occurrence in a page's body. [to_slug] is the
     slugified link target (which need not exist as a page yet), [label] the
     displayed text. The cascade only fires if foreign keys are enabled on the
     connection (off by default in SQLite); we also clear links explicitly on
     each save, so it is belt-and-suspenders for a future page-delete feature. *)
  let create_links_table =
    (Caqti_type.unit ->. Caqti_type.unit)
      {sql| CREATE TABLE IF NOT EXISTS links (
              id           INTEGER PRIMARY KEY AUTOINCREMENT,
              from_page_id INTEGER NOT NULL,
              to_slug      TEXT NOT NULL,
              label        TEXT NOT NULL,
              FOREIGN KEY (from_page_id) REFERENCES pages(id) ON DELETE CASCADE
            ) |sql}

  let create_links_index =
    (Caqti_type.unit ->. Caqti_type.unit)
      {sql| CREATE INDEX IF NOT EXISTS idx_links_to_slug
            ON links (to_slug) |sql}

  let delete_links_from =
    (Caqti_type.int ->. Caqti_type.unit)
      {sql| DELETE FROM links WHERE from_page_id = ? |sql}

  let insert_link =
    (Caqti_type.(t3 int string string) ->. Caqti_type.unit)
      {sql| INSERT INTO links (from_page_id, to_slug, label)
            VALUES (?, ?, ?) |sql}

  (* Pages that link to [to_slug], newest title order. Joining on
     from_page_id means only links from existing pages are returned. *)
  let backlinks =
    (Caqti_type.string ->* Caqti_type.(t2 string string))
      {sql| SELECT DISTINCT p.slug, p.title
            FROM links l
            JOIN pages p ON p.id = l.from_page_id
            WHERE l.to_slug = ?
            ORDER BY p.title |sql}
end

(* Each operation takes a connection module (Caqti packs the live connection
   into a first-class module) and returns a [result] inside an Lwt promise:
   [Ok _] on success, [Error e] carrying a Caqti error otherwise. *)

(* Sequence Lwt-wrapped Caqti results, stopping at the first error. Lets us
   chain several statements while propagating the first [Error]. *)
let ( let*? ) m f =
  match%lwt m with Error _ as e -> Lwt.return e | Ok x -> f x

let init (module Conn : Caqti_lwt.CONNECTION) =
  let*? () = Conn.exec Q.create_table () in
  let*? () = Conn.exec Q.create_links_table () in
  Conn.exec Q.create_links_index ()

let find_by_slug (module Conn : Caqti_lwt.CONNECTION) slug =
  Conn.find_opt Q.find_by_slug slug

(* Replace all outgoing links of a page with the given set. *)
let replace_links (module Conn : Caqti_lwt.CONNECTION) ~from_page_id links =
  let rec insert_all = function
    | [] -> Lwt.return (Ok ())
    | (link : Wiki_link.t) :: rest ->
        let*? () =
          Conn.exec Q.insert_link (from_page_id, link.slug, link.label)
        in
        insert_all rest
  in
  let*? () = Conn.exec Q.delete_links_from from_page_id in
  insert_all links

(* Upsert the page, then rebuild its outgoing links from the body's
   [[wiki links]], so backlinks stay in sync with the content. *)
let save (module Conn : Caqti_lwt.CONNECTION) ~slug ~title ~body =
  let*? () = Conn.exec Q.upsert (slug, title, body) in
  let*? page = Conn.find_opt Q.find_by_slug slug in
  match Option.bind page (fun (p : Page.t) -> p.id) with
  | None -> Lwt.return (Ok ())
  | Some from_page_id ->
      replace_links
        (module Conn : Caqti_lwt.CONNECTION)
        ~from_page_id (Wiki_link.extract body)

let list_all (module Conn : Caqti_lwt.CONNECTION) =
  Conn.collect_list Q.list_all ()

let backlinks (module Conn : Caqti_lwt.CONNECTION) slug =
  Conn.collect_list Q.backlinks slug

type pool = (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt_unix.Pool.t

(* Open a connection pool for the given URI, e.g. "sqlite3:wiki.db". The
   result is synchronous (not wrapped in Lwt): either the ready pool or a
   load error if the driver/URI is unusable. *)
let connect (uri : string) : (pool, Caqti_error.t) result =
  match Caqti_lwt_unix.connect_pool (Uri.of_string uri) with
  | Ok pool -> Ok pool
  | Error e -> Error (e :> Caqti_error.t)

(* Borrow a connection from the pool, run [f] with it, and return it. This is
   how callers should run the helpers above against a pool. *)
let use pool f = Caqti_lwt_unix.Pool.use f pool
