# CalculiTester
CalculiTester is a proof assistant that allows user to interactively proof
theorems with various simple rule-based deductive system using tactics,
inspired by Coq.

## Building
CalculiTester uses Cabal for building.

You might need to install `parsec`, `lens` and `haskeline` packages.

``` bash
cabal update
cabal install parsec lens haskeline
```

Then configure with

``` bash
cabal configure
```

And build with

``` bash
cabal build
```

Then CalculiTester can be run with

``` bash
cabal run
```

Or you can copy the file `dist/build/CalculiTester/CalculiTester` to a place
convenient for you and run it from there.

## Basic Usage

Basic usage of CalculiTester looks like this

``` bash
CT> Load theory $Theory$
$Theory$> Theorem $name$ : $expression$
...tactics
Theorem $name$ saved
```

For example, proving the Law of Identity using Łukasiewicz axioms can be
achieved with the following

``` bash
CT> Load theory theories/Axiomatic_Logic.th 
Loaded theory Axiomatic Logic
Axiomatic Logic> Theorem id : p -> p

p -> p
────────────────────────────────────────
Axiomatic Logic: id> apply mp

P -> p -> p
────────────────────────────────────────
	P
Axiomatic Logic: id> apply mp

P0 -> P -> p -> p
────────────────────────────────────────
	P0
	P
Axiomatic Logic: id> apply ax2

p -> Q1 -> p
────────────────────────────────────────
	p -> Q1
Axiomatic Logic: id> apply ax1

p -> Q1
────────────────────────────────────────
Axiomatic Logic: id> apply ax1
Theorem id saved
```

## Manual

CalculiTester has two modes of interactions: the main mode and the proof
mode.
CalculiTester starts in the main mode. In this mode user interacts with the
program using commands, which starts with a capital letter.
Commands instruct the program to load a deductive theory, save proofs and
view them, and so on.

By using the command `Theorem` user enters the proof
mode. In proof mode user interacts with program using tactics, which 
starts with a lower-case letter. A tactic instructs the program to attempt
to make some kind of progress on the proof.

In the proof mode the program displays a list of goals, which are the
statements to be proved. Initially this list is just the statement of
the theorem to be proved. Each tactic modifies the list of goals.
When the list of goals becomes empty, the theorem is considered to
be proved and the proof is saved. The program exits the proof mode if 
the theorem is proved or the user cancels the proof by pressing `Ctrl-D`

### Expressions

Statements are written using expressions. Internally an expression is either
a variable or an atom with a (possibly empty) list of expressions. 

Expressions are parsed according to the following grammar

``` 
expression = expression 0
expression n = expression m, infixl m, expression (m + 1) (* m = n *) |
               expression (m + 1), infixr m, expression m (* m = n *) |
               simpleExpression
simpleExpression = atom, { shell } | shell
shell = { prefix }, nucleus, { postfix }
nucleus = variable | atom | list | '(', expression, ')'
list = "[]" | '[', expression, ']' | 
       '[', { expression,  ';' }, expression ']'
```
`variable` is a sequence of non-whitespace characters starting with
an upper-case letter

`atom` is a sequence of non-whitespace characters starting with a digit or
a lower-case letter

`infixl m`, `infixr m`, `prefix`, `postfix` are operators, which are
represented by a sequence of characters starting with
non-alphanumeric character, except reversed sequence `;`

Whitespace between atoms, variables and operators is mandatory

For example:

`X`, `Y`, `Atom`, `Atom++`, `X+b`, `Μεταβλητη` are variables.

`x`, `y`, `var`, `var++`, `x+b`, `ατομο` are atoms.

`+`, `>>=`, `$f` are operators.

`infixl m` is a left-associative infix operator with precedence m.

`infixr m` is a right-associative infix operator with precedence m.

`prefix` is a prefix operator.

`postfix` is a postfix operator.

Properties of the operators are declared in the theory file. Operators 
with higher precedence bind tighter. Postfix operators are applied before
prefix. Operator `#` is reserved as a right associative operator with
precedence 0. It is used for interpretation of lists, redeclaring it's
properties is not recommended.

When an expression with an infix operator is read, it's interpreted with an
operator as the head atom of the expression. For example:

`X + Y` is represented internally as `Atom "+" [Var "X", Var "Y"]`

Lists are interpreted using operator `#` and the special atom `[]` in
the following way:

`[a; b; c; ... z]` is equivalent to `(a # b # c # ... # z # [])`

Examples (assuming usual precedence and associativity of arithmetic
operators): 

`X + Y * Z` is `Atom "+" [Var "X", Atom "*" [Var "Y", Var "Z"]]`

`(X + Y)^ N ^ M` is `Atom "^" [Atom "+" [Var "X", Var "Y"], Atom "^" [Var "N", Var "M"]]`

`take One and Two` is `Atom "take" [Var "One", Atom "and" [], Var "Two"]`

`- - 2 ^-1 ^-1` is `Atom "-" [Atom "-" [Atom "^-1" [Atom "^-1" [Atom "2" []]]]]`

### Theory file

Theory file describes a deductive theory, i.e. the properties of the
used operators and the deductive rules. Each kind of information is 
declared using a separate paragraph. Each paragraph consists of a heading
line and a sequence of non-empty lines. Paragraphs are separated by at least
one empty or whitespace line.

#### Theory name paragraph

`name: $name$`

This paragraph consists only of a heading. It states the name of theory,
which used in the prompt.

#### Prefix and postfix operators paragraphs

```
prefix:
$operators$
...
```

```
postfix:
$operators$
...
```

These paragraphs describe prefix or postfix operators. The heading determine
the kinds of the operators listed. The body contains the operators separated
by whitespace (possibly several lines)

Examples:

```
prefix:
- ~
¬
```

```
postfix:
^-1 ★
^op
```

#### Infix operators paragraph

```
infix:
$operators$ r $operators$
$operators$ r $operators$
...
```

This paragraph describes infix operators. First line lists operators with
precedence 1, second with precedence 2 and so on. Left- and
right-associative operators are separated with `r`. It's optional, if all
operators in the line are left-associative.

Example:

```
infix:
,
+ - r ⊕
* r ⊗
r ^
```

#### Rule paragraph

```
--$name$
$premise$
$premise$
...
$conclusion$
```

This paragraph describes a deductive rule. The heading consists of two
hyphens and the name of the rule. The body consists of one line for
each premise of the rule and one line for the conclusion. Atoms and
operators in the expressions for the premises and the conclusion denote
the invariable parts of the rule. The variables denote subexpressions that
depend on the application. The same variable in the conclusion and
premises denotes the same subterm during the application.

Examples: 

Modus ponens

```
--mp
P → Q
P
Q
```

usually written as

```
P → Q      P
────────────
      Q
```

Disjunction introduction rule

```
--I∨
Γ ⊢ A
Γ ⊢ A ∨ B
```

Excluded middle axiom

```
--LEM
A ∨ ¬ A
```

Successor constructor for natural numbers

```
--succ
Γ ⊢ X ∈ nat
Γ ⊢ s X ∈ nat
```

### Unification

Results of tactics depend on unification of certain expressions. 
Unification is process that calculates for two given expression, a
substution of variables, that make them identical. After unification
each variable in the given expression either belongs to an
equivalence class of varibles or equal to some expression. The result of
the unification is the substitution which substitute the variables either
with the alphabetically first in the equivalence class or the
corresponding expression.

### Commands

#### Load theory

```
Load theory $file$
```

This command load the thery from `$file$`. It overwrites the information
about properties of operators and the rule database. It deletes the
theorem database and name of the file used to save proofs.

It fails if `$file$` doesn't exists or if there is an error while
parsing the file.

#### Load proofs

```
Load proofs $file$
```

This command loads the proofs from `$file$`. It only loads the correct
proofs according to the currently loaded theory.

It fails if `$file$` doesn't exist or if there is an error while
parsing the file

#### Theorem

```
Theorem $name$ : $expression$
```

```
Theorem $name$: $expression$
```

This command remembers `$name$` as the name of the theorem to save later,
initializes the list of goals with the singleton containing `$expression$`
and starts the proof mode.

It fails if there is an error while parsing `$expression$`

#### Show proof

```
Show proof $name$
```

This command shows the proof of `$name$` as a tree of rule names. It uses the
Haskell function `drawTree`.

It fails if the theorem `$name$` is not in the database.

#### Draw proof

```
Draw proof $name$
```

```
Draw proof $name$ $file$
```

This command generates a representation of the proof of the `$name$` as
a proof tree. If the argument `$file$` is present, then the proof tree is
written to `$file$`, otherwise it's written to the output.

It fails if the theorem `$name$` is not in the database.

Example of a proof tree:

```
─────────────────────────────ax1  ─────────ax1
(P → Q → P) → R → (P → Q → P)     P → Q → P   
──────────────────────────────────────────────mp
              R → (P → Q → P)
```

#### Save proofs

```
Save proofs $proofs$
```

This command appends the proofs the save file. When it's called the
first time, it asks for the name of the file and remembers it
for the next time. It overwrites the file if it's not a file with
proofs.

It's not supposed to fail.

#### Exit

```
Exit
```

This command terminates the program

### Tactics

#### idtac

```
idtac
```

This tactic doesn't do anything.

#### apply

```
apply $name$
```

This tactic loads the rule `$name$` from the database. It tries to unify the
conclusion of the rule with the first goal, then replaces it with
the premises of the rule and apply the result of the unification to
all goals.

It fails if the rule is not in the database or if the unification fails.

Example:

If modus ponens is defined as

```
--mp
P → Q
P
Q
```

Then applying it gives the following result

```

P
──────────
	Q
	R
	S
Theory: theorem> apply mp

P0 → P
──────────
	P0
	Q
	R
	S
```


#### repeat

```
repeat $n$ $tactic$
```

This tactic repeats `$tactic$` n times. At each step it's applied to first
goal left after the previous steps.

It fails, if `$tactic$` fails at any step. After the failure the state
reverts to the one before calling `repeat`.

```
repeat $tactic$
```

This tactic repeats `$tactic$` until it fails. It leaves the state as it was
before the first failure. For performance reason, the number of repetions
is limited to 255.

It's not supposed to fail.

Examples:

If the implication introduction rule is defined as:

```
--I→
(A # G) ⊢ B
G ⊢ A → B
```

then following will happen

```
[] ⊢ a → b → c → d → e
────────────────────────────────────────
IPC: test> apply I→

[a] ⊢ b → c → d → e
────────────────────────────────────────
```

```
[] ⊢ a → b → c → d → e
────────────────────────────────────────
IPC: test> repeat 4 apply I→

[d; c; b; a] ⊢ e
────────────────────────────────────────
```

```
[] ⊢ a → b → c → d → e
────────────────────────────────────────
IPC: test> repeat 5 apply I→
Error: Can't unify atoms e and →

[] ⊢ a → b → c → d → e
────────────────────────────────────────
```

```
[] ⊢ a → b → c → d → e
────────────────────────────────────────
IPC: test> repeat apply I→

[d; c; b; a] ⊢ e
────────────────────────────────────────
```

#### hyp

```
hyp $name$
```

This tactic assumes existance of the rules `$name$_head` and `$name$_tail`.
Such rules can be used for searching for a proposition within a
context. The tactic applies the rule `$name$_head` and stops if it 
succeeds, otherwise it applies the rule `$name$_tail` and repeats.

It fails, if applying a rule fails at any step. It reverts the state
on failure.

```
hyp
```

The same as `hyp hyp`

Examples:

Assuming the definitions

```
--hyp_head
(A # T) ⊢ A

--hyp_tail
T ⊢ A
(H # T) ⊢ A
```

the following will happen

```

[a; b; c; d] ⊢ a
────────────────────
	r
Theory: theorem> hyp_head

r
────────────────────
```

```

[a; b; c; d] ⊢ c
────────────────────
	r
Theory: theorem> hyp_tail

[b; c; d] ⊢ c
────────────────────
	r
```

```

[a; b; c; d] ⊢ c
────────────────────
	r
Theory: theorem> hyp hyp

r
────────────────────
```

## Included theories

Directory `theories` contains some example theory files.

Axiomatic_Logic.th: Classical propositional calculus based on
Łukasiewicz axioms and modus ponens. Includes implication (`->`) and
negation (`~`).

IPC.th: Intutionistic sequent-based propositional calculus. Includes
implication (`->`), negation (`~`), conjunction (`&&`), disjunction (`||`),
truth (`true`) and falsity (`false`).

## Appendix

### Example proof

The proof that, in the intutionistic propositional calculus, 
implication as disjunction implies implication as negation of 
conjunction.

```
CT> Load theory theories/IPC.th 
Loaded theory IPC
IPC> Theorem implications : [] |- (~ a || b) -> ~ (a && ~ b)

[] |- ~a || b -> ~(a && ~b)
────────────────────────────────────────
IPC: implications> apply I->

[~a || b] |- ~(a && ~b)
────────────────────────────────────────
IPC: implications> apply I~

[~a || b] |- a && ~b -> false
────────────────────────────────────────
IPC: implications> apply I->

[a && ~b; ~a || b] |- false
────────────────────────────────────────
IPC: implications> apply E||

[a && ~b; ~a || b] |- A1 || B1
────────────────────────────────────────
	[A1; a && ~b; ~a || b] |- false
	[B1; a && ~b; ~a || b] |- false
IPC: implications> hyp

[~a; a && ~b; ~a || b] |- false
────────────────────────────────────────
	[b; a && ~b; ~a || b] |- false
IPC: implications> apply E->

[~a; a && ~b; ~a || b] |- A4 -> false
────────────────────────────────────────
	[~a; a && ~b; ~a || b] |- A4
	[b; a && ~b; ~a || b] |- false
IPC: implications> apply E~

[~a; a && ~b; ~a || b] |- ~A4
────────────────────────────────────────
	[~a; a && ~b; ~a || b] |- A4
	[b; a && ~b; ~a || b] |- false
IPC: implications> hyp

[~a; a && ~b; ~a || b] |- a
────────────────────────────────────────
	[b; a && ~b; ~a || b] |- false
IPC: implications> apply E&&L

[~a; a && ~b; ~a || b] |- a && B3
────────────────────────────────────────
	[b; a && ~b; ~a || b] |- false
IPC: implications> hyp

[b; a && ~b; ~a || b] |- false
────────────────────────────────────────
IPC: implications> apply E->

[b; a && ~b; ~a || b] |- A9 -> false
────────────────────────────────────────
	[b; a && ~b; ~a || b] |- A9
IPC: implications> apply E~

[b; a && ~b; ~a || b] |- ~A9
────────────────────────────────────────
	[b; a && ~b; ~a || b] |- A9
IPC: implications> apply E&&R

[b; a && ~b; ~a || b] |- A10 && ~A9
────────────────────────────────────────
	[b; a && ~b; ~a || b] |- A9
IPC: implications> hyp

[b; a && ~b; ~a || b] |- b
────────────────────────────────────────
IPC: implications> hyp
Theorem implications saved
```
