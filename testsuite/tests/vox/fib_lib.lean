-- Spec library for lean_fib.ml: Fibonacci over Int, totalized with
-- fib n = 0 for n <= 0.  [fib] itself is NOT @[grind]: grind sees it
-- only through the unfolding lemmas (fib_neg / fib_one / fib_rec) and
-- the fast-doubling identities, proved from the addition formula by
-- functional induction on a Nat-indexed helper.

def fibN : Nat -> Int
  | 0 => 0
  | 1 => 1
  | n + 2 => fibN n + fibN (n + 1)

def fib (n : Int) : Int := fibN n.toNat

@[grind =] theorem fib_neg (n : Int) (h : n <= 0) : fib n = 0 := by
  have h0 : n.toNat = 0 := by omega
  simp [fib, h0, fibN]

@[grind =] theorem fib_one (n : Int) (h : n = 1) : fib n = 1 := by
  subst h; simp [fib, fibN]

@[grind =] theorem fib_rec (n : Int) (h : 2 <= n) :
    fib n = fib (n - 1) + fib (n - 2) := by
  have h1 : n.toNat = (n - 2).toNat + 2 := by omega
  have h2 : (n - 1).toNat = (n - 2).toNat + 1 := by omega
  simp only [fib, h1, h2, fibN]
  grind

theorem fibN_add (m n : Nat) :
    fibN (m + n + 1) = fibN (m + 1) * fibN (n + 1) + fibN m * fibN n := by
  fun_induction fibN m with
  | case1 => simp [fibN]
  | case2 => grind [fibN]
  | case3 m ih1 ih2 => grind [fibN]

theorem fib_add (m n : Int) (hm : 0 <= m) (hn : 0 <= n) :
    fib (m + n + 1) = fib (m + 1) * fib (n + 1) + fib m * fib n := by
  have h1 : (m + n + 1).toNat = m.toNat + n.toNat + 1 := by omega
  have h2 : (m + 1).toNat = m.toNat + 1 := by omega
  have h3 : (n + 1).toNat = n.toNat + 1 := by omega
  simp only [fib, h1, h2, h3]
  exact fibN_add m.toNat n.toNat

@[grind =] theorem fib_double (k : Int) (hk : 0 <= k) :
    fib (2 * k) = fib k * (2 * fib (k + 1) - fib k) := by
  by_cases h : k = 0
  · subst h; simp [fib_neg]
  · have ha := fib_add (k - 1) k (by omega) hk
    have hr := fib_rec (k + 1) (by omega)
    have e1 : k - 1 + k + 1 = 2 * k := by omega
    have e2 : k - 1 + 1 = k := by omega
    have e3 : k + 1 - 1 = k := by omega
    have e4 : k + 1 - 2 = k - 1 := by omega
    rw [e1, e2] at ha
    rw [e3, e4] at hr
    grind

@[grind =] theorem fib_double_succ (k : Int) (hk : 0 <= k) :
    fib (2 * k + 1) = fib k * fib k + fib (k + 1) * fib (k + 1) := by
  have ha := fib_add k k hk hk
  have e1 : k + k + 1 = 2 * k + 1 := by omega
  rw [e1] at ha
  grind
