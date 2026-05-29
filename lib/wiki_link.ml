type t = { label : string; slug : string }

let re = Str.regexp "\\[\\[\\([^]]+\\)\\]\\]"

let extract body =
  let rec loop pos acc =
    try
      let _ = Str.search_forward re body pos in
      let label = Str.matched_group 1 body in
      let slug = Slug.slugify label in
      loop (Str.match_end ()) ({ label; slug } :: acc)
    with Not_found -> List.rev acc
  in
  loop 0 []

let equal a b = a.label = b.label && a.slug = b.slug

let pp fmt { label; slug } =
  Format.fprintf fmt "{ label = %S; slug = %S }" label slug

let testable = Alcotest.testable pp equal
