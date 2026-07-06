// vox syntax mode for CodeMirror 5.
//
// The stock `mllike` OCaml mode is blind to everything that makes vox
// vox: refinement braces and the `_` hole, ghost markers, the [@@vox.*]
// attribute family, `<-` assignment, and the embedded [%%vox.lean]
// blocks whose interior is Lean, not OCaml.  This mode keeps the
// vendored mllike pristine and layers vox on top of it:
//
//   * plain OCaml is delegated, verbatim, to a real mllike sub-mode, so
//     every OCaml token class the existing themes rely on is unchanged;
//   * vox-specific spans are intercepted BEFORE mllike sees them, but
//     only when mllike is at its base tokenizer (never mid string /
//     comment / quoted-string) so the delegation stays consistent;
//   * a [%%vox.lean {lean| ... |lean}] block switches to a small Lean
//     sub-tokenizer until its |lean} terminator.
//
// Token classes emitted on top of the mllike set (CodeMirror prefixes
// each with `cm-`):
//
//   vox-refine-delim   the { } of a refinement  int{ ... }
//   vox-hole           the _ hole inside a refinement  (the star)
//   vox-spec-app       a spec function in application head position
//   vox-quant          forall_ / exists_
//   vox-marker         a trailing-underscore ghost marker (total_, ...)
//   vox-assign         <-
//   vox-mutable        mutable
//   vox-attr           [@ [@@ [@@@ [% [%% attribute/extension delimiters
//   vox-attr-name      the attribute's dotted name (vox.decreases, ...)
//   vox-lean-delim     {lean| and |lean}
//   vox-lean-keyword   theorem def by have fun match ...
//   vox-lean-tactic    grind omega fun_induction induction simp rw ...
//   vox-lean-sort      Prop Type Sort Int Nat Bool VoxU ...
//   vox-lean-comment   -- line and /- -/ block comments

(function (mod) {
  if (typeof exports == "object" && typeof module == "object")
    mod(require("../../lib/codemirror")); // CommonJS (node token test)
  else if (typeof define == "function" && define.amd)
    define(["../../lib/codemirror"], mod);
  else mod(CodeMirror); // plain browser
})(function (CodeMirror) {
  "use strict";

  // ---- Lean sub-language --------------------------------------------------

  // Lean 4 keywords that read as structure, not tactics.
  var LEAN_KEYWORDS = wordSet(
    "theorem def lemma example axiom opaque abbrev instance structure " +
      "inductive class deriving where with match fun let rec mutual in do " +
      "if then else by have show suffices from calc obtain refine "  +
      "return open namespace section end variable universe attribute " +
      "extends"
  );

  // Tactics and terms invoked in `by ...` blocks.
  var LEAN_TACTICS = wordSet(
    "grind omega simp simp_all simp_arith dsimp rw rewrite subst intro " +
      "intros exact apply refine cases rcases obtain constructor induction " +
      "fun_induction by_cases split left right use exists existsi " +
      "assumption rfl trivial decide ring ring_nf linarith nlinarith " +
      "norm_num aesop tauto contradiction generalize clear revert " +
      "rename_i first repeat all_goals any_goals try skip sorry gcongr " +
      "positivity field_simp conv unfold change"
  );

  // Sorts and library types.
  var LEAN_SORTS = wordSet(
    "Prop Type Sort Int Nat Bool Char String List Option Array Fin " +
      "True False VoxU"
  );

  function wordSet(s) {
    var out = Object.create(null);
    s.split(/\s+/).forEach(function (w) {
      if (w) out[w] = true;
    });
    return out;
  }

  // A single Lean token.  Assumes state.inLean is true.  Handles the
  // |lean} terminator, line/block comments, strings, numbers, the := and
  // <;> combinators, the common Lean unicode operators, and the keyword /
  // tactic / sort classification of identifiers.
  function leanToken(stream, state) {
    // Continuing a /- ... -/ block comment (they nest).
    if (state.leanComment > 0) {
      while (!stream.eol()) {
        if (stream.match("/-")) {
          state.leanComment++;
          continue;
        }
        if (stream.match("-/")) {
          state.leanComment--;
          if (state.leanComment === 0) break;
          continue;
        }
        stream.next();
      }
      return "vox-lean-comment";
    }
    if (stream.eatSpace()) return null;
    // The block terminator hands control back to OCaml.
    if (stream.match("|lean}")) {
      state.inLean = false;
      return "vox-lean-delim";
    }
    if (stream.match("--")) {
      stream.skipToEnd();
      return "vox-lean-comment";
    }
    if (stream.match("/-")) {
      state.leanComment = 1;
      return "vox-lean-comment";
    }
    var ch = stream.peek();
    if (ch === '"') {
      stream.next();
      var escaped = false,
        c;
      while ((c = stream.next()) != null) {
        if (c === '"' && !escaped) break;
        escaped = !escaped && c === "\\";
      }
      return "string";
    }
    if (/\d/.test(ch)) {
      stream.next();
      stream.eatWhile(/[\w.]/);
      return "number";
    }
    // Multi-char operators, then the common Lean unicode.
    if (stream.match(":=") || stream.match("<;>") || stream.match("=>")) {
      return "vox-lean-op";
    }
    if (/[·∀∃¬→←↔∧∨⟨⟩λΠΣ×≤≥≠∘∈∉⊢]/.test(ch)) {
      stream.next();
      return "vox-lean-op";
    }
    if (/[A-Za-z_]/.test(ch)) {
      stream.next();
      // Lean identifiers can be dotted and carry subscripts/primes.
      stream.eatWhile(/[\w'.!?ₐ-ₜ₀-₉]/);
      var word = stream.current();
      // A dotted name keys on its final segment for tactic/kw lookup so
      // e.g. `Nat.rec` is not miscoloured, but `grind` still lands.
      var head = word.indexOf(".") >= 0 ? word : word;
      if (LEAN_KEYWORDS[head]) return "vox-lean-keyword";
      if (LEAN_TACTICS[head]) return "vox-lean-tactic";
      if (LEAN_SORTS[head]) return "vox-lean-sort";
      return null; // plain names use the default foreground
    }
    stream.next();
    return null;
  }

  // ---- vox overlay on OCaml ----------------------------------------------

  var OCAML_WORDS = wordSet(
    // mllike's keyword + builtin + type vocabulary; kept here only to
    // avoid recolouring these as spec applications / markers.
    "as do else end exception fun functor if in include let of open rec " +
      "struct then type val while with and assert begin class constraint " +
      "done downto external function initializer lazy match method module " +
      "mutable new nonrec object private sig to try value virtual when " +
      "raise failwith true false asr land lor lsl lsr mod or not " +
      "int float bool char string unit List"
  );

  // Is the character run ending at `idx` (exclusive) preceded, ignoring
  // spaces, by a value-producing character?  Used both to tell a
  // refinement brace `int{` from a record brace `{ x = ... }`, and to
  // tell an application head from an argument inside a refinement.
  function prevIsValue(str, idx) {
    var i = idx - 1;
    while (i >= 0 && (str.charAt(i) === " " || str.charAt(i) === "\t")) i--;
    if (i < 0) return false;
    return /[\w')\]]/.test(str.charAt(i));
  }

  // Is what follows the identifier of length `wlen` (starting at the
  // stream's current, un-consumed position) the beginning of an argument?
  // A space then a value-starter, or an immediately-abutting `(`.
  function nextIsArg(stream, wlen) {
    var after = stream.string.slice(stream.pos + wlen);
    return /^\s+[\w'(]/.test(after) || /^\(/.test(after);
  }

  // The vox interceptor.  Runs only when the OCaml sub-mode is at its base
  // tokenizer (see token()).  Returns a token class if it claimed a span,
  // or null to let the OCaml sub-mode tokenize normally.
  function voxIntercept(stream, state) {
    var start = stream.start;
    var str = stream.string;

    // Inside a refinement predicate: the delimiters, the hole, quantifiers
    // and spec applications are ours; everything else is ordinary OCaml.
    if (state.refine > 0) {
      var ch = stream.peek();
      if (ch === "{") {
        stream.next();
        state.refine++;
        return "vox-refine-delim";
      }
      if (ch === "}") {
        stream.next();
        state.refine--;
        return "vox-refine-delim";
      }
      if (stream.match(/^_(?![\w'])/)) return "vox-hole";
      if (stream.match(/^(?:forall_|exists_)(?![\w'])/)) return "vox-quant";
      if (/[a-z_]/.test(ch)) {
        var m = stream.match(/^[a-z_][\w']*/, false);
        if (m) {
          var w = m[0];
          if (OCAML_WORDS[w]) return null; // not, mod, true, ...
          if (/_$/.test(w) && w.length > 1) {
            stream.match(/^[a-z_][\w']*/);
            return "vox-marker";
          }
          // Application head: not itself an argument, and followed by one.
          if (!prevIsValue(str, start) && nextIsArg(stream, w.length)) {
            stream.match(/^[a-z_][\w']*/);
            return "vox-spec-app";
          }
        }
      }
      return null;
    }

    // Pending attribute name (the token after an opener like [@@).
    if (state.attrName) {
      state.attrName = false;
      if (stream.match(/^[A-Za-z_][\w.]*/)) return "vox-attr-name";
      // Opener not followed by a name: fall through to normal handling.
    }

    // A [%%vox.lean {lean| ... |lean}] block: switch to Lean at {lean|.
    if (stream.match(/^\{lean\|/)) {
      state.inLean = true;
      return "vox-lean-delim";
    }

    // Attribute / extension openers: [@  [@@  [@@@  [%  [%%
    if (stream.match(/^\[(?:@{1,3}|%{1,2})/)) {
      state.attrName = true;
      return "vox-attr";
    }

    // <- assignment (mllike would split it into two operator tokens).
    if (stream.match(/^<-/)) return "vox-assign";

    var c = stream.peek();

    // A refinement brace abuts the type it refines (int{, varr{, ){ ),
    // where a record brace is preceded by whitespace or `=`.  A {| quoted
    // string is left to mllike.
    if (c === "{") {
      if (str.charAt(stream.pos + 1) === "|") return null; // {|...|}
      if (prevIsValue(str, start)) {
        stream.next();
        state.refine = 1;
        return "vox-refine-delim";
      }
      return null;
    }

    // Identifiers: ghost markers (trailing _) and the quantifier binders,
    // recognised anywhere.  Everything else is ordinary OCaml.
    if (/[a-z_]/.test(c)) {
      var im = stream.match(/^[a-z_][\w']*/, false);
      if (im) {
        var iw = im[0];
        if (iw === "for") {
          // mllike's OCaml vocabulary omits `for`; supply it so loop
          // headers colour like the `to` / `downto` / `done` around them.
          stream.match(/^for/);
          return "keyword";
        }
        if (iw === "mutable") {
          stream.match(/^mutable/);
          return "vox-mutable";
        }
        if (OCAML_WORDS[iw]) return null;
        if (iw === "forall_" || iw === "exists_") {
          stream.match(/^[a-z_][\w']*/);
          return "vox-quant";
        }
        if (/_$/.test(iw) && iw.length > 1) {
          stream.match(/^[a-z_][\w']*/);
          return "vox-marker";
        }
      }
    }
    return null;
  }

  CodeMirror.defineMode("vox", function (config) {
    var ocaml = CodeMirror.getMode(config, "text/x-ocaml");
    // The reference to mllike's base tokenizer, so token() can tell when
    // the OCaml sub-mode is mid string / comment (do not intercept then).
    var ocamlBase = CodeMirror.startState(ocaml).tokenize;

    return {
      startState: function () {
        return {
          ocaml: CodeMirror.startState(ocaml),
          inLean: false,
          leanComment: 0,
          refine: 0,
          attrName: false,
        };
      },
      copyState: function (s) {
        return {
          ocaml: CodeMirror.copyState(ocaml, s.ocaml),
          inLean: s.inLean,
          leanComment: s.leanComment,
          refine: s.refine,
          attrName: s.attrName,
        };
      },
      token: function (stream, state) {
        if (state.inLean || state.leanComment > 0)
          return leanToken(stream, state);
        // Never intercept while the OCaml sub-mode is inside a multi-line
        // string / comment / quoted-string: let it finish first.
        var atBase =
          state.ocaml.tokenize === ocamlBase &&
          state.ocaml.commentLevel === 0 &&
          !state.ocaml.longString;
        if (!atBase) return ocaml.token(stream, state.ocaml);
        if (stream.eatSpace()) return null;
        var vox = voxIntercept(stream, state);
        if (vox !== null) return vox;
        return ocaml.token(stream, state.ocaml);
      },
      innerMode: function (state) {
        // Expose the active sub-language so CodeMirror's own helpers
        // (indent, bracket matching) see Lean inside a block and OCaml
        // outside it.  The Lean interior has no stateful sub-mode, so it
        // reports as this mode (null inner means "self").
        return state.inLean ? null : { state: state.ocaml, mode: ocaml };
      },
      blockCommentStart: "(*",
      blockCommentEnd: "*)",
    };
  });

  CodeMirror.defineMIME("text/x-vox", "vox");

  // Exposed for the headless token test: a tiny runMode over this mode,
  // returning [text, class] pairs.  Uses only public CodeMirror API
  // (getMode / startState / StringStream), so it works in the browser and
  // under a DOM stub in node.
  CodeMirror.voxTokenize = function (text) {
    var mode = CodeMirror.getMode({ indentUnit: 2, tabSize: 2 }, "vox");
    var state = CodeMirror.startState(mode);
    var out = [];
    var lines = text.split("\n");
    for (var i = 0; i < lines.length; i++) {
      var stream = new CodeMirror.StringStream(lines[i], 2, {
        lookAhead: function () {
          return null;
        },
        baseToken: function () {
          return null;
        },
      });
      if (lines[i] === "" && mode.blankLine) mode.blankLine(state);
      while (!stream.eol()) {
        var style = mode.token(stream, state);
        out.push([stream.current(), style || null]);
        stream.start = stream.pos;
      }
      if (i < lines.length - 1) out.push(["\n", null]);
    }
    return out;
  };
});
