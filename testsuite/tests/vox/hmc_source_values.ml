module D = Hm_declarative

type value =
  | True | False
  | Word of Hmc_word64.t
  | Nil | Cons of value * value
  | Closure of D.term * value
  | Recursive_closure of D.term * value
  | Empty | Bind of value * value
  [@@inductive]
