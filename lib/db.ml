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
end

(* Each operation takes a connection module (Caqti packs the live connection
   into a first-class module) and returns a [result] inside an Lwt promise:
   [Ok _] on success, [Error e] carrying a Caqti error otherwise. *)

let init (module Conn : Caqti_lwt.CONNECTION) = Conn.exec Q.create_table ()

let save (module Conn : Caqti_lwt.CONNECTION) ~slug ~title ~body =
  Conn.exec Q.upsert (slug, title, body)

let find_by_slug (module Conn : Caqti_lwt.CONNECTION) slug =
  Conn.find_opt Q.find_by_slug slug

let list_all (module Conn : Caqti_lwt.CONNECTION) =
  Conn.collect_list Q.list_all ()

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
