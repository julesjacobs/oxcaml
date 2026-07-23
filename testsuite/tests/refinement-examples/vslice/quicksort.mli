val sort_array :
  array:Vslice.array @ unique ->
  Vslice.array{
    Vslice_model.sorted (Vslice.contents _) = true
    && Vslice_model.perm (Vslice.contents array) (Vslice.contents _) = true
  } @ unique

val demo :
  unit ->
  Vslice.array{
    Vslice_model.sorted (Vslice.contents _) = true
  } @ unique

val parallel_sort_array :
  array:Vslice.array @ unique ->
  Vslice.array{
    Vslice_model.sorted (Vslice.contents _) = true
    && Vslice_model.perm (Vslice.contents array) (Vslice.contents _) = true
  } @ unique

val parallel_demo :
  unit ->
  Vslice.array{
    Vslice_model.sorted (Vslice.contents _) = true
  } @ unique
