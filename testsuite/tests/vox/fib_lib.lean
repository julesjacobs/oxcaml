-- Lemma library for lean_fib.ml, stated about the REFLECTED [fib]
-- (the compiler emits its definition just above this prelude): the
-- fast-doubling identities, proved from the fib addition formula by
-- functional induction on a Nat-indexed helper and transported along
-- the bridge [fib n = fibN n.toNat].

def fibN : Nat -> Int
  | 0 => 0
  | 1 => 1
  | n + 2 => fibN n + fibN (n + 1)

theorem fibN_add (m n : Nat) :
    fibN (m + n + 1) = fibN (m + 1) * fibN (n + 1) + fibN m * fibN n := by
  fun_induction fibN m with
  | case1 => simp [fibN]
  | case2 => grind [fibN]
  | case3 m ih1 ih2 => grind [fibN]

theorem fib_eq_fibN (n : Int) : fib n = fibN n.toNat := by
  fun_induction fib n with
  | case1 x h =>
    have h0 : x.toNat = 0 := by omega
    simp [h0, fibN]
  | case2 x => simp [fibN]
  | case3 x h1 h2 ih1 ih2 =>
    have e0 : x.toNat = (x - 2).toNat + 2 := by omega
    have e1 : (x - 1).toNat = (x - 2).toNat + 1 := by omega
    rw [e0, fibN, ih1, ih2, e1]
    grind

theorem fibN_rec (m : Nat) : fibN (m + 2) = fibN m + fibN (m + 1) := by
  simp [fibN]

theorem fibN_double (m : Nat) :
    fibN (2 * m) = fibN m * (2 * fibN (m + 1) - fibN m) := by
  match m with
  | 0 => simp [fibN]
  | k + 1 =>
    have ha := fibN_add k (k + 1)
    have e : 2 * (k + 1) = k + (k + 1) + 1 := by omega
    have hr := fibN_rec k
    rw [e, ha]
    grind

theorem fibN_double_succ (m : Nat) :
    fibN (2 * m + 1) = fibN m * fibN m + fibN (m + 1) * fibN (m + 1) := by
  have ha := fibN_add m m
  have e : m + m + 1 = 2 * m + 1 := by omega
  rw [e] at ha
  grind

@[grind =] theorem fib_double (k : Int) (hk : 0 <= k) :
    fib (2 * k) = fib k * (2 * fib (k + 1) - fib k) := by
  have e1 : (2 * k).toNat = 2 * k.toNat := by omega
  have e2 : (k + 1).toNat = k.toNat + 1 := by omega
  simp only [fib_eq_fibN, e1, e2]
  exact fibN_double k.toNat

@[grind =] theorem fib_double_succ (k : Int) (hk : 0 <= k) :
    fib (2 * k + 1) = fib k * fib k + fib (k + 1) * fib (k + 1) := by
  have e1 : (2 * k + 1).toNat = 2 * k.toNat + 1 := by omega
  have e2 : (k + 1).toNat = k.toNat + 1 := by omega
  simp only [fib_eq_fibN, e1, e2]
  exact fibN_double_succ k.toNat
