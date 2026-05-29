let is_slug_char = function 'a' .. 'z' | '0' .. '9' -> true | _ -> false

let slugify title =
  title |> String.lowercase_ascii
  |> String.map (function
    | 'a' .. 'z' as c -> c
    | '0' .. '9' as c -> c
    | _ -> '-')
  |> String.split_on_char '-'
  |> List.filter (fun part -> part <> "")
  |> String.concat "-"
