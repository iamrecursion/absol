# Metalanguage Design Notes

## Ground Truths for Termination
These can either be:
- Embedded in the compiler 
- Generic and contained at the top of the file 

The generic mechanism provides a more extensible solution as it would allow a
DSL designer to specify known truths relevant to their language that the
designer of the compiler did not consider.

The compiler could then uses these truths in the proof mechanism, rather than
utilising an embedded subset of known terminating operations.

## Initial Goals
- Semantic rules have some number of hypotheses and some conclusion.
- Initially start with no state, and then head for error state -> functional.
- Some semantic construct for each constructor of an AST node. 
- Typed lambda calculus discipline + recursive style semantics.
- Representing well-founded recursion over finite data (effective iteration)
- Type system with inductive data-types + map/reduce 
- --> catamorphism / anamorphism

### Catamorphism
- Allows a definition of things like map and reduce in terms of the language 
constructs while allowing termination proofs

## Language Features
- Static typing, syntactic typing, simple typing
- Provision of useful, inductively defined data structures.
- Functions 
- Higher order functions
- Basic user-defined operations
- Syntax is fully user defined, and so is semantics
- Syntactic functions (special forms)

## DSL Format
The DSL will be specified using a metalanguage.

### Cherrypicking
Mechanism for cherrypicking of provided features should use a 'using' directive.
This is to allow disambiguation of terms that the DSL designer may choose to 
include in the syntax of their language.

If there is `using x`, then x becomes a reserved keyword in the language, and 
cannot be used as a variable or keyword in the user-defined portion of the
language.

### Ground-Truth Semantics
These are the basic semantic constructs of the DSL. They should be specified in
a nameless fashion as a list, using the Metalanguage syntax for defining 
semantic constructs. 

These 'ground-truth' semantics, so to speak, are those used by the proof engine
of the metacompiler to verify that all possible programs in this language will
terminate.

### Syntax and Semantics Interleaving
The syntactic portion of this specification will be based on EBNF, but with an
interleaved set of semantic rules. 

While a basic rule in EBNF is of the following form, usually specified on a
single line:

```
<syntax-element> ::= <terminal> | <expr> 
```

The interleaving of semantics will require a form more like:

```
<syntax-element> ::= 
    <expr> --> { associated semantic expression } | 
    <terminal> --> { associated ground-truth expression };
```

The definition of additional syntax in this grammar (to be specified as an EBNF)
does not preclude the use of similar syntax in the language itself. New syntax:

- `-->` Indicates the association between a piece of syntax and its associated
    semantic block.
- `{}` Provides a block for specifying the semantics, using syntax-element 
    capture from the associated syntax block.
- `;` Terminates the specification statement.

### Semantic Definition Format
Semantics definition through a transformation of Big-Step operational semantics.
The essence of the definition is to use a 'where' like definition `:` to 
specify the recursive structure of the rule.

- Sub-terms of the definition are defined recursively using successive 'where'
    clauses specified by `:`.
- Pattern matches on elements of the syntax. 

For example, if we consider a rule for addition of two terms:

```
(M_1 V n_1) (M_2 V n_2) | (M_1 + M_2 V n) :: n = n_1 + n_2
```

This can be transformed into the following associated with a syntax term of form
`M1 + M2`:

```
M1 + M2 --> { n : (n = n1 + n2) : (n1 <= M1), (n2 <= M2) };
```

NB: May instead want to use braces rather than parentheses.
Any term in the semantics that are not pattern matched on the non-terminals of 
the syntax rule are considered as semantic.

Potentially want to think about the 'values' in the language. These are the
things that can be found on the RHS of the evaluation relation (downarrow). 

The language will provide some syntactic special forms to aid the definition of
the language semantics:

- Function application
- Mapping
- Traversal

Consider a rule for function application (abstraction):

```
--> { V : (V <= M'[N/x]), (\x.M' <= M) }
```

This provides limited scope for definition of semantics, but allows the
restriction to languages that can be shown to terminate. 

#### Issues with the Current Work
There are a few main issues with this syntax specification system that need to 
be resolved:

- It is difficult to determine how to indicate function application in a 
    flexible manner that accurately reflects the behaviour.
- There is some difficulty in distinguishing between semantic information and 
    pattern matches from the syntax in the semantic specifications.
- Probably want to provide the ability to omit the semantic specification in  
    some cases (e.g. terminals), as these do not necessarily need semantics.
    + Should look at how this interacts with numbers at a basic level, but need 
        to examine more complex types as well.
- There is no sensible way to actually execute these trivial programs... what 
    should that look like? 
- What does executability of a statement look like from this perspective? 
    + How do results come out of just a 'program'? 
- How on earth do you pattern match on an arbitrary function definition? 
- How to distinguish between terminals that have semantics and terminals that 
    don't? More accurately, which terminals have no EXECUTION semantics.

#### Notes on the Above
- Consider each node in the AST as a constructor with arguments of kind_1...
    kind_n. These can then be matched on from a positional perspective. 
- Positional addressing by kind to specify the matches: matches first on all 
    terms of kind $k$, and then position within that kind $k$[p] where $p$ is 
    the position of the term.
- Evaluation of the semantic 'binding' is conditional on the correct evaluation 
    of the conditions (in parentheses).
- Named function calls require 'e' the environment:
    + The environment is a well-typed store (Bindable)
    + Addressable by some kind of key (effectively a map)
    + This can contain function bindings, identifiers, errors, etc. 
    + No nested scope in functions (for now).
    + All variables in the function body should initially be formal parameters
        of the function. 
    + Function calls operate via the 'funcall' mechanism which is a syntactic
        convenience for the metaspec author to denote how their function syntax
        maps onto semantics
    + The actual semantics of 'funcall' are TBC, but could be:
        * The Lambda calculus behaviour
        * Use mutable environments (an environment stack), and execute directly.
        * SECD Machine, Krivine Machine
- Unscoped code is implicitly put into a main.
- Simplify typing:
    + Omit partial function application -> all the types that are checked are
        ground. So this is simple. 

```haskell
map :: (a -> b) -> [a] -> [b]
map :: [a] -> [b]
```

### Layout
The DSL Metalanguage layout will consist of:

1. A documentation block specifying:
    - Name: The name of the language.   
    - Version: The language version (all files must have a language version
    specified for which they can be built).
    - Using: A block that specifies the cherrypicked system-level dependencies
    of this DSL.
2. A block specifying the 'ground-truth' semantics for the language. 
3. A block containing the language specification itself.

It will use the `.meta` file extension. 
