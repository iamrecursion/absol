# Metacompiler Design Notes

## Proof Mechanism
The level of sophistication of the proof mechanism is very much up for
discussion. 

- Prove it in the compiler
- Use a unification-style mechanism?
- Or a simple pattern-matcher mechanism

Need to check:

- That the rules only use the allowable mechanisms
- Checking they have complete coverage
- Everything decomposes to termination in the cases of recursion
- Or matches one of the allowable rules. 

## Some Concerns
Regarding lexing and parsing of metaspec, this can be done without really
knowing the details of how the proof mechanism is intended to work. 

- Worth having a separate lexer stage? 
- Or let the parsec lexer handle it? 

## Primitive Operations
Basically ending up with defining a list of Primitive operations that can be
applied within the semantics. 

## Grammar Transformations
Left-recursive grammars may pose a problem. How to handle this:

- Avoid it by stipulating non-left-recursive grammars. 

## Typing
Typed at the level of semantics, parsing can associate the typing information.
This can then be type-checked by pattern matching on the types contained in the
node meta-information. 
