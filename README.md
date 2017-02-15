# ABSOL
This project provides a system for the creation of formally correct
Domain-Specific Languages (DSLs).
It is the **A**utomatic **B**uilder for **S**emantically **O**riented
**L**anguages.

## About ABSOL
ABSOL is a metalanguage (Metaspec) and metacompiler (ABSOL) system used for the 
creation of formally verified and correct DSLs.
It allows the user to fully specify both the syntax and semantics of their DSL,
before verifying the DSL correctness and generating a compiler for that new 
language. 

The system provides a powerful set of features for DSL creation, with a focus 
on DSL execution semantics, but does not provide for Turing-Complete languages,
as without this restriction the proof engine would not function. 
These languages can be tailored to the domain at hand, and interfaced with via
the host language's Foreign-Function Interface (FFI). 

The project was conceived as the topic of my dissertation at the University of
Bath. 

### Proving Semantic Correctness
In order to allow the proof of semantic correctness for ABSOL-based DSLs, 
Metaspec places restrictions on the kinds of semantic operations that the user 
can use in their DSL.
To this end, semantics are restricted to those which are structurally recursive.
This means that all of the terms composing the semantics must consist of
structural sub-terms.

While this would be an onerous restriction in the context of General-Purpose 
programming languages, the restricted feature-set of most DSLs means that this
is not really an issue. 

While one may be concerned that such a restriction greatly compromises the power
of the DSLs that could be created, the language augments this proof mechanism
with additional features that have been manually proven to terminate in all
cases. 
These include:

- **Environments:** Environment access to allow for stateful computation, the
definition of functions and other useful features.
- **Function Calls:** Systems for defining and calling functions in the DSL.
- **Semantic Typing:** The typing discipline of any defined DSL is enforced by
the way the DSL semantics are defined, freeing the DSL makers from needing to 
mess with typing themselves. 
- **Flexible Semantic Syntax:** Allowing restricting semantic evaluations on the
results of sub-evaluations, and clear syntax for defining how the semantic
results are produced. 
- **Direct Syntax Access:** The semantics can address portions of the syntactic
language grammar, allowing for intuitive definitions of structural semantics.
- **Flexible Data Types:** A multitude of useful data types, including maps, 
arrays, linked-lists and matrices, as well as simple raw types. 
- **Data-Traversal Functionality:** Methods for working with the included 
data-types. 

## Using ABSOL
This repository contains all the information that you need to get started with 
ABSOL. 

1. The metacompiler can be built from the stack project.
2. The ABSOL libraries can be found in the `/src` directory, and accompanying
haddock documentation can be built.
3. The syntax and semantics of Metaspec can be found in the dissertation 
document itself. 

### Defining a Language
The user must specify the syntax of their DSL using the EBNF-variant syntax 
provided as part of metaspec.
An example follows:

```
<function-call> ::= <identifier> "(" <param-list> ")" ";"
```

The user then specifies the semantics associated with each portion of the
syntax, using the metaspec semantic-definition syntax.
An example follows:

```
<function-call> ::= <identifier> "(" <param-list> ")" ";" --> {
    any n : {n = n1}() : {
        any n1 <= funcall(
            e.<identifier>[0].<statement>[0],
            <param-list>[0],
            e.<identifier>[0].<param-decl-list>[0]
            )
    } 
};
```

Another, full example:

```
<ifthen> ::= 
    "if" <condition> "then" <statement> "else" <statement> --> {
        any n : {n = n2}(n1 == true) : 
            {bool n1 <= <condition>[0]}, 
            {any n2 <= <statement>[0]} |
        any n : {n = n2}(n1 == false) : 
            {bool n1 <= <condition>[0]}, 
            {any n2 <= <statement>[1]}
    };
```

## FAQs

**Q: Are ABSOL DSLs Turing-Complete?**  
No. This restriction is in place to make the task of proving semantic 
correctness possible. If ABSOL allowed for Turing-Complete programming language
creation, it would be impossible in general to show that all programs in these
languages terminate. 

**Q: Is ABSOL named after a Pokémon?**  
A: Yes, definitely. The repository was sitting around for a while, unused, so
I created a backronym for it to fit with the project. 
