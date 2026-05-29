type t = {
  id : int option;
  slug : string;
  title : string;
  body : string;
  created_at : string option;
  updated_at : string option;
}

let make ?id ?created_at ?updated_at ~slug ~title ~body () =
  { id; slug; title; body; created_at; updated_at }
