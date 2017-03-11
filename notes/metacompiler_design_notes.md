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

## Semantic Checker Algo
Features that the algorithm needs to have:

- Needs to check that it is total
- Descent through every path to show they all terminate
- Needs to check that semantics validate on the restricted form
- Needs to check that all cases are covered where restrictions are present (this
  may include a catchall case), cases evaluated in order
- Check that all non-terminals referred to in each rule have been defined.

### Basic Idea
Needs to perform a graph traversal, marking each node that has been shown to 
terminate. This allows for short circuiting later evaluations if a given node 
has already been shown to terminate.  


## Compiler TODOs
These were things done as part of the compiler design, including the parser and
verification engine. They will provide things to write and reason about in the
dissertation:

- Initially unordered toplevel defs and productions changed to ordering:
	+ This sacrifices flexibility for the ability to do single-pass stateful
	parses. 
	+ This allows the parser to perform a series of verifications on the spec
	that are required for correctness (and not related to the semantic) 
	verification engine.
