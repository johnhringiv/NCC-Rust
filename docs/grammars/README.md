# NCC Output Grammars

TextMate grammars for syntax-highlighting NCC's debug/intermediate output. They
mirror what the compiler actually prints, so pasted compiler dumps can be
colourised instead of guessed.

**Primary consumer: [johnhringiv.com](https://johnhringiv.com).** The website
highlights ncc output in blog write-ups with [Shiki](https://shiki.style/) using
the **vitesse-dark** theme. The website copy of these grammars is kept identical
to this folder; treat this repo as the source of truth and copy downstream.

| File | Highlights | Produced by |
| --- | --- | --- |
| [`tokens.json`](tokens.json) | `Processed tokens: [...]` token dump | `ncc file.c --lex` |
| [`ast-tree.json`](ast-tree.json) | The tree-view IRs at every stage | `ncc file.c --parse` / `--validate` / `--tacky` / `--codegen` |
| [`package.json`](package.json) | VS Code / JetBrains bundle manifest so the folder imports as a TextMate bundle | — |

## Source of truth

These grammars are derived from the code, not vice-versa. When the relevant
enums or pretty-printer change, update the grammar:

- **`tokens.json`** ← the `Token` enum in [`src/lexer.rs`](../../src/lexer.rs).
  Tokens are printed with Rust's `{:?}`, so the matched words are the exact
  variant names (`IntKeyword`, `Negation`, `BitwiseLeftShiftAssign`, …) and the
  payload forms `Identifier("…")` / `ConstantInt("…")` etc.

- **`ast-tree.json`** ← the `ItfDisplay` implementations in
  [`src/pretty.rs`](../../src/pretty.rs), plus the enums they format
  (`parser`, `validate`, `tacky`, `codegen`).

## Colour scheme ⇄ `pretty.rs`

`src/pretty.rs` colours the tree view for the **terminal** (ANSI truecolor);
these grammars re-colour the *same* text for the **web** (Shiki, vitesse-dark).
They are meant to agree, so the grammar's scope names track the vitesse-dark
roles. The authoritative palette (RGB constants + which node gets which colour)
lives in the doc comment at the top of `pretty.rs`. In summary:

| Role | Colour | Examples |
| --- | --- | --- |
| Declarations | teal | `Program`, `FunctionDefinition`, `VarDeclaration` |
| Instructions / opcodes & expression nodes | gold | `Mov`, `Binary`, `Copy`, `Cast`, `Assignment` |
| Control flow | green | `Return`, `If`, `While`, `Conditional`, `None` |
| Operators & condition codes | muted red | `Add`, `Sub`, `NotEqual`, `E`, `NE`, `LE` |
| Names & jump labels (incl. `Data` global name) | olive | function names, `-> break_loop.0` |
| Locals / temporaries & `Pseudo` payload | cream | `temp.1`, `i.1` |
| Numbers / offsets & `Imm` immediates | cyan | `-4`, `Imm(3)` |
| Registers & `Pseudo` keyword | indigo | `AX`, `R10`, `SP`, `XMM0` |
| Memory-operand keywords | tan | `Stack(…)`, `Data(…)` |

The key idea is that **operands are coloured by storage class** — immediate
(cyan), register (indigo), memory (tan) — so a spill to memory (the expensive,
interesting event when reading codegen) stands out at a glance.

Two intentional parity gaps between the grammar and `pretty.rs`:

- Parser `Constant(Int(0))` literals are gold in `pretty.rs` but teal
  (`constant.numeric`, the number/immediate family) in the grammar — the gold
  isn't reachable from a flat vitesse scope, and teal is the more internally
  consistent choice.
- Flat text can't distinguish some nodes the AST can: e.g. a temporary on the
  right of `->` (`Copy 5 -> i.1`) is coloured like a jump label, because targets
  share the `name.N` shape (`break_loop.0`) and the two are indistinguishable
  without the tree.

### Potential: align `-S` with this palette

The `-S` flag (emit x86-64 assembly) is a **separate** colour path — it renders
real assembly via iced-x86's formatter, mapped to named 16-colour ANSI in
`get_color` (`src/main.rs`), not the vitesse truecolor constants above. It isn't
consumed by these grammars (assembly is highlighted with a stock `asm` grammar
downstream), so nothing needs to change. But the two terminal views disagree on
the same concepts — registers are indigo here vs `blue` in `-S`, instructions
gold vs `magenta` mnemonics, labels olive vs `green` (numbers already agree at
cyan). A future consistency pass could point `get_color` at these constants
(Register→indigo, Mnemonic→gold, Number→cyan, Label→olive), which would require
exposing the palette from `pretty.rs` (make the `const`s `pub` or lift them into
a shared module). Fine as-is for now.

## Multiple compiler versions, on purpose

Blog snippets are **pinned to the compiler version each post is written about** —
they are *not* refreshed to track `ncc`. So the grammar must highlight several
historical output formats at once, permanently. This is a standing design
constraint, not tech debt:

- `AllocateStack` / `DeallocateStack` — older stack-allocation form (current
  `ncc` emits `Binary<Q> Sub Imm(n) -> SP`). Kept as an always-on rule.
- `Constant(0)` — older bare parser-literal form (current `ncc` emits
  `Constant(Int(0))`). The parser-constant rule matches both.

When the compiler changes an output format, **add** the new form to the grammar;
don't replace the old one, or posts about earlier versions lose their
highlighting.

## Notes / known limitations

- TextMate matching is first-match-wins per position, so order matters:
  payload operands (`Imm(…)`, `Stack(…)`, …), size markers (`<L>`/`<Q>`/`<D>`),
  registers, and the `Label(…)` / `FunctionDefinition:` headers are matched
  before the bare keyword lists. In particular a register destination
  (`Mov R10 -> AX`) is matched *before* the arrow-target rule so it is coloured
  as a register, not a jump label.
- Trailing `# …` annotations are matched as comments, so hand-annotated pastes
  (`Copy a -> b   # note`) don't mis-highlight words inside the note.
- `Return` prints as a statement (parser/validate) and as a TACKY instruction;
  both are highlighted with one scope. The single-letter condition codes
  (`E`, `A`, `B`, `G`, `L`) are matched as whole words after the
  `<L>`/`<Q>`/`<D>` size markers have been consumed.
- The grammars only cover language features NCC currently implements
  (`int`/`long`/`unsigned int`/`unsigned long`/`double`, the operators and
  statements in the source, the SSE conversion/`XMM` machinery for `double`).
  No pointers, structs, chars, etc. — add them here when the compiler grows
  them.

## Regenerating / sanity-checking

```bash
cargo build --release
./target/release/ncc file.c --tacky | sed 's/\x1b\[[0-9;]*m//g'   # strip ANSI to see raw labels
```

Compare the bare node/keyword words against the alternations in
`ast-tree.json`; every emitted label should fall into exactly one scope.
