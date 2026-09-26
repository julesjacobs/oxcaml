type t = {
  tag : int;
  payload : int;
  first : int;
  second : int;
  third : int;
}

let[@def] (equal @ total) (x : t @ immutable) (y : t @ immutable) =
  x.tag = y.tag && x.payload = y.payload && x.first = y.first
  && x.second = y.second && x.third = y.third

let[@def] (mix @ total) (x : int) (y : int) =
  x * 31 + y

let[@def] (hash @ total) (key : t @ immutable) =
  mix (mix (mix (mix key.tag key.payload) key.first) key.second) key.third

let (reflexive @ total) (x : t @ immutable) :
    {u : unit | equal x x} =
  equal_def x x; ()

let (symmetric @ total) (x : t @ immutable) (y : t @ immutable) :
    {u : unit | equal x y = equal y x} =
  equal_def x y; equal_def y x; ()

let (transitive @ total) (x : t @ immutable) (y : t @ immutable)
    (z : t @ immutable) :
    {u : unit | not (equal x y && equal y z) || equal x z} =
  equal_def x y; equal_def y z; equal_def x z; ()

let (hash_equal @ total) (x : t @ immutable) (y : t @ immutable) :
    {u : unit | not (equal x y) || hash x = hash y} =
  equal_def x y;
  hash_def x;
  hash_def y;
  ()

let (exact @ total) (x : t @ immutable) (y : t @ immutable) :
    {u : unit | equal x y = (x === y)} =
  equal_def x y; ()
