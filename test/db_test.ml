open Lwt.Infix

(* Caqti operations return [(_, error) result Lwt.t]. [unwrap] waits for the
   promise and turns an [Error] into a raised exception, so a DB failure fails
   the test loudly instead of being silently ignored. *)
let unwrap m = m >>= Caqti_lwt.or_fail

(* Open a fresh in-memory SQLite database, create the schema, hand the
   connection to [f], and drive the whole Lwt promise to completion
   synchronously so it fits Alcotest's plain [unit -> unit] test functions.

   We use a single direct connection (not [Db.connect]'s pool) because every
   pooled connection to "sqlite3::memory:" would get its own separate, empty
   database. *)
let with_db f =
  Lwt_main.run
    (let%lwt conn =
       unwrap (Caqti_lwt_unix.connect (Uri.of_string "sqlite3::memory:"))
     in
     let%lwt () = unwrap (Microwiki.Db.init conn) in
     f conn)

let test_save_and_find () =
  with_db (fun conn ->
      let%lwt () =
        unwrap
          (Microwiki.Db.save conn ~slug:"home" ~title:"Home" ~body:"Welcome")
      in
      let%lwt found = unwrap (Microwiki.Db.find_by_slug conn "home") in
      (match found with
      | Some page ->
          Alcotest.(check string) "title" "Home" page.Microwiki.Page.title;
          Alcotest.(check string) "body" "Welcome" page.Microwiki.Page.body
      | None -> Alcotest.fail "expected to find the saved page");
      Lwt.return_unit)

let test_find_missing_returns_none () =
  with_db (fun conn ->
      let%lwt found =
        unwrap (Microwiki.Db.find_by_slug conn "does-not-exist")
      in
      Alcotest.(check bool) "absent" true (Option.is_none found);
      Lwt.return_unit)

let test_upsert_updates_existing () =
  with_db (fun conn ->
      let%lwt () =
        unwrap (Microwiki.Db.save conn ~slug:"p" ~title:"First" ~body:"a")
      in
      let%lwt () =
        unwrap (Microwiki.Db.save conn ~slug:"p" ~title:"Second" ~body:"b")
      in
      let%lwt all = unwrap (Microwiki.Db.list_all conn) in
      Alcotest.(check int) "still one row" 1 (List.length all);
      let%lwt found = unwrap (Microwiki.Db.find_by_slug conn "p") in
      (match found with
      | Some page ->
          Alcotest.(check string)
            "updated title" "Second" page.Microwiki.Page.title;
          Alcotest.(check string) "updated body" "b" page.Microwiki.Page.body
      | None -> Alcotest.fail "expected to find the upserted page");
      Lwt.return_unit)

let test_list_all_orders_by_title () =
  with_db (fun conn ->
      let%lwt () =
        unwrap (Microwiki.Db.save conn ~slug:"b" ~title:"Banana" ~body:"")
      in
      let%lwt () =
        unwrap (Microwiki.Db.save conn ~slug:"a" ~title:"Apple" ~body:"")
      in
      let%lwt all = unwrap (Microwiki.Db.list_all conn) in
      let titles = List.map (fun p -> p.Microwiki.Page.title) all in
      Alcotest.(check (list string))
        "ordered by title" [ "Apple"; "Banana" ] titles;
      Lwt.return_unit)

let () =
  Alcotest.run "db"
    [
      ( "pages",
        [
          Alcotest.test_case "save and find" `Quick test_save_and_find;
          Alcotest.test_case "find missing returns None" `Quick
            test_find_missing_returns_none;
          Alcotest.test_case "upsert updates existing" `Quick
            test_upsert_updates_existing;
          Alcotest.test_case "list_all orders by title" `Quick
            test_list_all_orders_by_title;
        ] );
    ]
