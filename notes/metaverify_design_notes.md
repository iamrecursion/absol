# Metaverify Design
Metaverify is the name used to refer to the algorithm and framework that is used
to determine if all programs in a given language terminate. 

## Semantic Checker Requirements
The semantic checker deals with verifying the termination properties of the 
semantics provided for the input language. It performs a traversal of all NTs
defined for the language, and marks each node that has been shown to terminate.
This means that it has to verify the following properties of the language 
semantics:

- **Totality:** The semantics should be defined for all possible descents down
  the semantic tree.
- **Termination:** The algorithm needs to descend through all semantic
  evaluation paths for each of the non-terminals with semantics. In doing so it
  will be able to verify (predicated on the other conditions being true) that
  the language is guaranteed to terminate. This will occur until the guaranteed
  truths are reached. If this rule is satisfied, then it is not possible for a
  computation in this language to 'hang', with no defined semantics. There are 
  two kinds of semantics to check:
    + *Defined Semantics:* In the cases where the syntax has a semantic block
      defined for it, the checker must verify that all sub-evaluations terminate
      (whether they fall into recursively defined or special case semantics).
    + *Undefined Semantics:* There are some non-terminal symbols where there may
      not be predefined semantics for the symbol. This will require implicit 
      rules on the form of the syntax. 
- **Complete Guards:** All guards must cover the entire domain over which they 
  are defined. Guards are evaluated in order, and must include enough cases to
  cover the domain. This does not preclude the creation of specific-case guards.
- **Semantic Form Validation:** All rules must be of the form where the 
  sub-evaluations must be syntactic sub-expressions of the main rule. If this is
  satisfied, it guarantees that computations cannot diverge. 
- **Recognition of Special Cases:** Special case semantics are those that cannot
  be proved by the recursive mechanism. These are assumed to terminate by the 
  proof algorithm, and are to be proven elsewhere (ahead of time) in a manual
  fashion. 
- **Definition of NT Symbols:** All non-terminal symbols must be defined, and
  they must be defined only once. This work is actually performed at parse-time,
  but is crucial for the correctness of the termination proof. 

In combination, these rules ensure that all programs defined in this language
are able to terminate. 

Additionally, the language needs to be able to deal with cycles in the rules, 
both in the case of direct and indirect recursion in rules. This is graph based.

It assumes that rules cannot recurse through the start rule. This is no real
limitation as language designers can just indirect through a non-terminal if
needed. 

## The Semantic Checker Algorithm
The basic idea of this algorithm is to perform a traversal of a graph formed 
from the definitions of all non-terminals, including the start rule. 

**Input:** `SemanticRules = [NT Parse Trees] [Truths] Start Rule`   
**Output:** `Bool` _true_ if the language is total, _false_ otherwise. 

```haskell
verifyLanguage :: SemanticRules -> (Either Bool Error)
```

A basic (iterative) sketch of the algorithm is as follows:

1. Choose the start rule as the initial rule.
2. For each rule in the semantics:
    1. If the rule has no semantics defined:
        1. Decompose the syntax of the NT
        2. If the syntax is of a form that can produce termination IF the NTs in
           it terminate, then mark it as terminating.
        3. Else mark as potentially non-terminating.
        4. Recursively check the NTs in the rule body.
    2. If the rule has semantics defined:
        1. Verify the form of the semantics is such that it matches the semantic
           form validation criterion above.
        2. Check the sub-terms for special-case syntax being properly used.
        3. If the sub-terms contain NTs or correct uses of special-case syntax:
            1. For the NTs recursively check that they terminate (skipping along
               the list if needed, and also checking against the truths block).
        4. Check that the guards are sufficient to cover the entire domain of 
           the semantics (no incomplete guards).
        5. If these criteria are satisfied, mark the NT as terminating.
        6. Otherwise mark the NT as potentially non-terminating.
3. If all NTs for the language have been marked as terminating, output true.
4. If there are rules that do not terminate, output an error report.

The error report should trace the non-termination for every rule that does not
terminate (e.g. Production `<foo>` does not terminate due to indirect dependency
on `<bar>` which does no terminate (via `<foo> => <baz> => <bang> => <bar>`)).
Both forms of non-termination (divergence, hang) should be reported as such.

### Verifying the Sub-Term Criterion
Considering the form of the rules as follows:

```metaspec
<arith-expr> "+" <arith-expr> --> {
    num n : {n = n1 + n2}() :
        {num n1 <= <arith-expr>[0]}, {num n2 <= <arith-expr>[1]}
}
```
To determine if this criterion is satisfied it is sufficient to check that:

- The output variable(s) occur only on the left hand side of the leftmost 
  semantic assignment.
- The sub-term variables (or derived operations) occur on the on the right side
  of the leftmost semantic assignment. 
- The sub evaluations depend only syntactic sub-terms of the production (or that
  they depend on any special-case piece of syntax).

### The Syntax-Only Cases
For each kind of production sub-term, the following rules can be applied:

- **Alternation:** Terminates if all branches of the alternation terminate.
- **Grouping:** Terminates if all terms in the group terminate. 
- **Exception:** Terminates if the class to the left side of the minus is known
  to terminate (exception acts purely on a syntactic level). 
- **Repetition (Multiply):** Terminates if the class of the multiplied NT is 
  known to terminate.
- **Repetition (Grouped):** Terminates if the repeated group is known to
  terminate.
- **Optional:** Terminates if the optional group is known to terminate 
  (sequencing).

Productions consisting only of terminals should either have semantics defined 
for them (hence not falling into this case), or should be listed in the `truths`
language defblock.

This guarantees enough information to prove the totality of the language.
