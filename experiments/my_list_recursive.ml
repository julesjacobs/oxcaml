type 'a my_list : immutable_data with 'a =
  | Nil
  | Cons of 'a * 'a my_list

module type S_alias = sig
  type 'a my_list' : immutable_data with 'a = 'a list
end

module Alias : S_alias = struct
  type 'a my_list' : immutable_data with 'a = 'a list =
    | []
    | ( :: ) of 'a * 'a my_list'
end

type 'a my_list2 : immutable_data with 'a = Nil2 | Cons2 of 'a * 'a foo
and 'a foo = 'a my_list2

