external ( = ) : 'a -> 'a -> bool = "%equal"

module Effect = struct
  type 'a t = 'a eff = ..

  external perform : 'a t -> 'a = "%perform"

  module Deep = struct
    type nonrec ('a, 'b) continuation = ('a, 'b) continuation

    let continue (_ : ('a, int) continuation) (_ : 'a) = 0
  end
end
