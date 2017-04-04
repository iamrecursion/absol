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
  Each guard can have multiple conditions, but each condition must have only
  one operator.
- **Semantic Form Validation:** All rules must be of the form where the 
  sub-evaluations must be syntactic sub-expressions of the main rule. Checking 
  this also involves checking the form of the evaluations. If this is satisfied, 
  it guarantees that computations cannot diverge. This process also needs to 
  check where variables are used. It also needs to check that no variable is
  defined more than once.
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

#### Evolution of the Syntax-Only Cases
While it is technically possible to automatically derive language semantics in
many cases of syntax-only rules, the mechanism results in significant increases
in complexity. The fact that this problem exists means that the grammar is 
potentially under-constrained for the restricted semantics, allowing 
representation of language concepts that cannot necessarily be encoded. 

- In most cases where syntactic expressions consist of some chained terminals 
  and non-terminals, it is not possible to infer the semantics of a rule in
  the general case. Even if some inference rule were created that took the 
  semantics of the first non-terminal, this would be nonsensical in most cases.
- In order, however, to support language writers, there is a limited form of 
  semantic inference that can be created.

This form of semantic inference operates purely on top-level syntax definitions.

- If the syntax rule consists purely of an alternation of non-terminals, then
  the semantics for that rule can be inferred. 
- While this could be extended to more complex rules, with some instances of 
  grouping, repetition and optional syntax, it is complex to do so.
- In the cases for which this semantic inference operates (as above), the 
  semantics of that rule are defined as the semantics of whichever non-terminal
  matches. 
- This means that such a rule has defined semantics IF each of the alternation 
  terms have defined semantics. 

While this is a limitation for automatically deriving semantics, this is less of
an issue due to the following factors:

- The metalanguage is designed explicitly with the intent of letting the 
  language designers define the semantics for their language.
- As a result, it is inappropriate to attempt to infer incorrect semantics in
  cases where it may not be clear.

However, the need for this restriction is an indication that the metaspec 
language grammar is under-constrained when it comes to specifying the syntax of
these languages. In an ideal world it would be context-sensitive, forcing users
to define semantics if the syntax rule doesn't match the expected format. 

## Handling Special Syntax
While most special syntax forms are trivial to handle, those contained in 
`funcall` are more complex to deal with.

- Is it possible to verify this at language compile time? 
- Doesn't seem so -> this is a flaw with the language-level verification 
  approach if my intuition is true.
- Alternative constraints could be devised, with the wrong combination compiling
  to a noop. Nevertheless, this is program-level verification, and hence is not
  quite in accordance with the project philosophy. 
- To this end, I don't think that `callproc` even needs to exist.

## Misc Notes
Some uncategorised notes:

- While the grammar will allow it to be defined (hence under-constrained), the
  creation of alternatives with semantics within other alternatives is entirely
  ignored. Ideally, the grammar would be refactored to disallow this case, but 
  in the sake of expediency for the project it is just ignored.

## Notes on Checking Guard Completeness
Checking completeness of guards is the halting problem in the general case, and 
it is a significantly non-trivial task here. However, the restricted set of 
guard operations allowed by metaspec makes the task less difficult. The process
is as follows:

1. Check the guards for form: If the guards contain expressions with multiple
   operations or nonsensical guards, an error is emitted.
2. Normalise the guards by performing the following translation:
  
      `!=` -> `<`, `>`
      `<=` -> `<`, `=`
      `>=` -> `>`, `=`

   And additionally placing all variables on the right hand side of the 
   expression. This translation takes place within the given guard.
3. There is now a simple set of patterns to match, so we group patterns keyed on
   the left and right hand side of each expression and an ordering is imposed 
   on these groups. This ordering is arbitrary and it doesn't matter how it is
   created.
4. For groups $g_0, \dots, g_n$, where $g_i = (l op r | op \in \{<, =, >\})$, we 
   need to have a situation where for each element in $g_i$, all three elements
   in $g_{i+1}$ exist. A catch-all clause can be substituted for any set of 
   guards, allowing correctness. 
5. If this condition is satisfied, then the guards are complete.

This algorithm is incredibly inefficient (with factorial complexity), but the 
problem itself seems to be NP-complete as the only way to guarantee completeness
is to perform an exhaustive check (maybe some proof of equivalence to some 
NP-complete problem to come).

### Over-Constrained Guards
The suspicion here is that this checking method over-constrains the form of
valid guard patterns. This is confirmed by a set of guards as follows, for
variables `n1`, `n2`:

    (n1 <= 2, n2 == 2)
    (n1 <= 2)
    (n1 > 2)

This set of guards would not be allowed by the algorithm above. To allow useful
guards like this, the algorithm requires alteration as follows:

1. Check the guards for form: If the guards contain expressions with multiple
   operations or nonsensical guards, an error is emitted.
2. Expand/Normalise the guards by performing the following translation:
  
      `!=` -> `<`, `>`
      `<=` -> `<`, `=`
      `>=` -> `>`, `=`

   And additionally placing all variables on the right hand side of the 
   expression. This translation takes place within the given guard.
3. Categorise guard expressions based on the left and right operands. If there
   are multiple expressions with the same operands in one pattern separate into
   independent patterns. Example:

      `(n1 < 2, n1 > 2)` -> `(n1 < 2)`, `(n1 > 2)`

   This translation attaches the same semantic rule to the 'ghost' pattern.
4. Insert holes into patterns such that each pattern has a place for each
   variable seen on the left of an restriction expression. 
5. Check that for each 'kind' of restriction expression (keyed by the left and
   right operands), there is either a complete set of guards or a 'hole' that
   corresponds to that category of restriction. These 'holes' act as catch-all
   clauses. If this restriction is satisfied, then the pattern guards are 
   complete. 

While these examples have been in terms of a variable and a constant, this 
algorithm should work just as well for relations between variables: `(n1 < n2)`.

#### An Algorithmic Example - Rejection
Consider a set of guards over variables `n1, n2, n3` as follows:

    (n1 <= n2)
    (n3 == 3)

    |
    V

    (n1 < n2, n1 == n2)
    (n3 == 3)

    |
    V

    (n1 < n2)
    (n1 == n2)
    (n3 == 3)

    |
    V

    (n1 < n2, ())
    (n1 == n2, ())
    ((), n3 == 3)

    ACCEPT

This result is incorrect, and so the algorithm is incorrect.

### Further Changes
As shown by the counterexample above, the revised algorithm is also incorrect. 
It would suffice to add an additional restriction that there either must be a 
complete set for each 'kind' of restriction, or that there must be catch-all
hole. 

This appears like it might be doubly exponential in the number of variables and
singly exponential in the number of equations. 

To this end, the best solution for the scope of the project appears to be the 
enforcement of a catch-all clause. While this guarantees that the guards are 
complete over their domain, it is particularly inelegant as it requires the 
language designers to write semantics for a clause that may never be executed.

Consider the example of the if statement, where this restriction emerges as a
significant flaw of the verification engine. It should be noted, however, that
this is not a flaw of the language Metaspec itself, and so future work could 
certainly attempt to impose a more rigorous checking methodology. 

## Typechecking
While this is, for the most part, deferred until language compile time, rather
than metalanguage check time, it may be possible to perform some type-related
checks at the language level. 

Whether or not this is the case, Metaspec provides some features to aid this.
These include the typed evaluations for the truths, and the type annotations
throughout the semantics. 

## Verification of Mutually-Recursive Rules
It is possible, as part of the syntax, to write rules that are mutually 
recursive. Consider the following example:

  <a> ::= <c> | <b> ;
  <b> ::= <a> | <d> ;

Assuming that both `<c>` and `<d>` have well-defined semantics, this language 
also has well-defined semantics. 

However, the issue arises when actually performing this verification as there
is a cycle that has to be resolved. The initial attempt to break this mutually
recursive ruleset involves storing a 'production stack', so to speak, that
tracks the current path through the productions.

This process is sufficient to break the potentially infinite recursion, not
revisiting a node that is a parent of the current NT in the verification graph.

However, continuing the above example, this leaves a situation where both rules
are potentially undefined as when in <b>, checking <a> will show 'untouched'. As
'untouched' is signalling, meaning that all super-nodes become 'untouched' by 
proxy, this does not work.

An initially obvious solution is to mark a node as 'touched' at the start of its
evaluation. Touched would not be signalling, and hence `touched && terminates`
takes the value `terminates`. It is initially unclear as to whether this process
is sound, so let's work an example. Using the above productions.

1. We enter `<a>`, marking it as Touched.

    <a> = touched
    <b> = untouched
    <c> = untouched
    <d> = untouched

2. We evaluate `<c>`, which gets the value terminates:

    <a> = touched
    <b> = untouched
    <c> = terminates
    <d> = untouched

3. We enter `<b>`, which gets the value touched:

    <a> = touched
    <b> = touched
    <c> = terminates
    <d> = untouched

4. In `<b>` we initially observe `<a>` again. The stack trace prevents us from 
   recursing back to check `<a>` again (and hence prevents infinite recursion).
5. We then continue on to `<d>`, which we can verify as terminating. It hence 
   gets the value Terminates:

    <a> = touched
    <b> = touched
    <c> = terminates
    <d> = terminates

6. As we return back up the call stack, `<b>` takes the value of the conjunction 
   of its subterms (terminates && touched) = terminates.

    <a> = touched
    <b> = terminates
    <c> = terminates
    <d> = terminates

7. Finally, `<a>` gets the value 'terminates' as both of its subterms terminate.

As the value precedence of the tags is as follows:

- Untouched
- DoesNotTerminate
- Terminates
- Touched

Any case where a rule is shown NOT to terminate will correctly propagate back up
the stack of productions, without any interference by the value 'touched'. 

This only works because the language itself is necessarily finite (and would
not parse), which means that there is always a concrete set of syntax, and hence
a concrete set of semantics associated with it. 
