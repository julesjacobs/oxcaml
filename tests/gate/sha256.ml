(* Minimal pure-OCaml SHA-256 (FIPS 180-4). Not performance-critical: the gate hashes
   short canonical query strings to key the oracle cache. Kept small and
   obviously-correct; validated against published test vectors in [selftest].

   32-bit arithmetic is done in [int] (OCaml native int is >= 63 bits) with an explicit
   [& 0xFFFFFFFF] mask after every operation that can overflow 32 bits. *)

let mask = 0xFFFFFFFF
let ( &&& ) a b = a land b
let ( +% ) a b = a + b &&& mask
let rotr x n = ((x land mask) lsr n) lor (x lsl (32 - n)) &&& mask
let shr x n = (x land mask) lsr n

let k =
  [|
    0x428a2f98;
    0x71374491;
    0xb5c0fbcf;
    0xe9b5dba5;
    0x3956c25b;
    0x59f111f1;
    0x923f82a4;
    0xab1c5ed5;
    0xd807aa98;
    0x12835b01;
    0x243185be;
    0x550c7dc3;
    0x72be5d74;
    0x80deb1fe;
    0x9bdc06a7;
    0xc19bf174;
    0xe49b69c1;
    0xefbe4786;
    0x0fc19dc6;
    0x240ca1cc;
    0x2de92c6f;
    0x4a7484aa;
    0x5cb0a9dc;
    0x76f988da;
    0x983e5152;
    0xa831c66d;
    0xb00327c8;
    0xbf597fc7;
    0xc6e00bf3;
    0xd5a79147;
    0x06ca6351;
    0x14292967;
    0x27b70a85;
    0x2e1b2138;
    0x4d2c6dfc;
    0x53380d13;
    0x650a7354;
    0x766a0abb;
    0x81c2c92e;
    0x92722c85;
    0xa2bfe8a1;
    0xa81a664b;
    0xc24b8b70;
    0xc76c51a3;
    0xd192e819;
    0xd6990624;
    0xf40e3585;
    0x106aa070;
    0x19a4c116;
    0x1e376c08;
    0x2748774c;
    0x34b0bcb5;
    0x391c0cb3;
    0x4ed8aa4a;
    0x5b9cca4f;
    0x682e6ff3;
    0x748f82ee;
    0x78a5636f;
    0x84c87814;
    0x8cc70208;
    0x90befffa;
    0xa4506ceb;
    0xbef9a3f7;
    0xc67178f2;
  |]

let digest_bytes (msg : string) : string =
  let h =
    [|
      0x6a09e667;
      0xbb67ae85;
      0x3c6ef372;
      0xa54ff53a;
      0x510e527f;
      0x9b05688c;
      0x1f83d9ab;
      0x5be0cd19;
    |]
  in
  let len = String.length msg in
  (* Padding: 0x80, then zeros, then 64-bit big-endian bit length, to a multiple of 64
     bytes. *)
  let bitlen = len * 8 in
  let padlen =
    let rem = (len + 1) mod 64 in
    if rem <= 56 then 56 - rem else 120 - rem
  in
  let total = len + 1 + padlen + 8 in
  let b = Bytes.make total '\000' in
  Bytes.blit_string msg 0 b 0 len;
  Bytes.set b len '\x80';
  (* 64-bit big-endian bit length in the final 8 bytes. *)
  for i = 0 to 7 do
    let shift = (7 - i) * 8 in
    Bytes.set b (total - 8 + i) (Char.chr ((bitlen lsr shift) land 0xff))
  done;
  let w = Array.make 64 0 in
  let nblocks = total / 64 in
  for blk = 0 to nblocks - 1 do
    let base = blk * 64 in
    for t = 0 to 15 do
      let o = base + (t * 4) in
      w.(t) <-
        (Char.code (Bytes.get b o) lsl 24)
        lor (Char.code (Bytes.get b (o + 1)) lsl 16)
        lor (Char.code (Bytes.get b (o + 2)) lsl 8)
        lor Char.code (Bytes.get b (o + 3))
    done;
    for t = 16 to 63 do
      let s0 =
        rotr w.(t - 15) 7 lxor rotr w.(t - 15) 18 lxor shr w.(t - 15) 3
      in
      let s1 = rotr w.(t - 2) 17 lxor rotr w.(t - 2) 19 lxor shr w.(t - 2) 10 in
      w.(t) <- w.(t - 16) +% s0 +% w.(t - 7) +% s1
    done;
    let a = ref h.(0)
    and bb = ref h.(1)
    and c = ref h.(2)
    and d = ref h.(3)
    and e = ref h.(4)
    and f = ref h.(5)
    and g = ref h.(6)
    and hh = ref h.(7) in
    for t = 0 to 63 do
      let bigS1 = rotr !e 6 lxor rotr !e 11 lxor rotr !e 25 in
      let ch = !e land !f lxor (lnot !e land !g &&& mask) in
      let t1 = !hh +% bigS1 +% ch +% k.(t) +% w.(t) in
      let bigS0 = rotr !a 2 lxor rotr !a 13 lxor rotr !a 22 in
      let maj = !a land !bb lxor (!a land !c) lxor (!bb land !c) in
      let t2 = bigS0 +% maj in
      hh := !g;
      g := !f;
      f := !e;
      e := !d +% t1;
      d := !c;
      c := !bb;
      bb := !a;
      a := t1 +% t2
    done;
    h.(0) <- h.(0) +% !a;
    h.(1) <- h.(1) +% !bb;
    h.(2) <- h.(2) +% !c;
    h.(3) <- h.(3) +% !d;
    h.(4) <- h.(4) +% !e;
    h.(5) <- h.(5) +% !f;
    h.(6) <- h.(6) +% !g;
    h.(7) <- h.(7) +% !hh
  done;
  let out = Bytes.create 32 in
  for i = 0 to 7 do
    let v = h.(i) in
    Bytes.set out (i * 4) (Char.chr ((v lsr 24) land 0xff));
    Bytes.set out ((i * 4) + 1) (Char.chr ((v lsr 16) land 0xff));
    Bytes.set out ((i * 4) + 2) (Char.chr ((v lsr 8) land 0xff));
    Bytes.set out ((i * 4) + 3) (Char.chr (v land 0xff))
  done;
  Bytes.unsafe_to_string out

let to_hex (b : string) : string =
  let buf = Buffer.create (String.length b * 2) in
  String.iter
    (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c)))
    b;
  Buffer.contents buf

let hex_digest (s : string) : string = to_hex (digest_bytes s)

(* Known-answer tests from FIPS 180-4 / common vectors. Returns [Ok ()] or a list of
   failures. *)
let selftest () : (unit, string list) result =
  let vectors =
    [
      ("", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855");
      ("abc", "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
      ( "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
        "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1" );
      ( String.make 1000000 'a',
        "cdc76e5c9914fb9281a1c7e284d73e67f1809a48a497200e046d39ccc7112cd0" );
    ]
  in
  let failures =
    List.filter_map
      (fun (input, expected) ->
        let got = hex_digest input in
        if String.equal got expected then None
        else
          Some
            (Printf.sprintf "sha256(%s): expected %s got %s"
               (if String.length input > 16 then
                  Printf.sprintf "<%d bytes>" (String.length input)
                else Printf.sprintf "%S" input)
               expected got))
      vectors
  in
  match failures with [] -> Ok () | fs -> Error fs
