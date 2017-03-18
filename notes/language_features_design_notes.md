# Language Features Design
This section contains the design requirements for the language features, 
sketched mentions of semantics, and formal specifications of what each feature
includes.

The parser currently leaves the operations unchecked for types.

## Operations on Features
All features, however they are implemented, should support the following ops:

- Implement features at the grammar level.
- Push types, constructors and special syntax into the grammar.
- Mapping from Parser-Level feature to Metaspec Feature
- Getting the types that they import
- Getting the keywords/special syntax that they import
- Getting the operations that they allow 
- Submitting a string to them and checking if they define something (to allow 
  for useful hints).

## Feature List
The language supports a whole host of features. However, for the sake of the
implementation, a minimal set of language features will be implemented.

These are:

- base: Basic non-terminal classes for parsing only, types like any, none.
- number: A variety of integer types and their operations, IEEE754 floats and 
  doubles, and their operations.
- string: A UTF-8 string type and associated operations.
- list: A list-based homogeneous container type and its operations.
- matrix: A n-dimensional matrix type with varied addressing modes
- traverse: Map, Fold, Filter
- funcall: Utilities for calling and defining the two kinds of function.

Additional language features to be considered as future work are:

- associative-array: An associative array or map type and its operations.
- random: Random number generation and associated operations.
- maybe: A maybe type, similar to Haskell.
- either: An either type, similar to Haskell.
- state: A user-controlled state (separate from the language environment). 
- special-number: Complex and rational numbers, arbitrary precision floats.
- math: Sets of additional mathematical operations defined for both `number` and
  `special-number`.

## Implementation Notes
This section contains notes on each of the features that have been chosen to be
implemented for this proof-of-concept.
Each section contains the following details:

- **Import Name:** This is the syntax placed in the using list to import these
  features into scope.
- **Types:** Any types that the feature brings into scope, with descriptions.
- **Non-Terminals**: Any non-terminals brought into scope by the feature. These
  will be defined later in the dissertation itself.
- **Operations:** The semantic operations that this feature allows on the types
  that it imports. This includes any special syntax.

### Base
**Import Name:** base

**Types:**
- any: A type marker that is substituted for an actual type at compile time.
- none: Suppresses statement output. 
- bool: A standard boolean type.

**Non-Terminals:**
- `<digit>` --- Digits
- `<nondigit>` --- All utf-8 nondigit characters excluding newlines.
- `<utf-8-char>` --- All allowable utf-8 characters. 

**Operations:**
- `&&, ||, *, |, ==, !=, <, >, <=, >=`

### Number
**Import Name:** number  

**Types:**  
- natural: An unbounded, unsigned integer type.
- integer: An unbounded, signed integer type.
- int32: A 32-bit signed integer type.
- uint32: An unsigned 32-bit integer type.
- int64: A signed 64-bit integer type.
- uint64: An unsigned 64-bit integer type.
- float: An IEEE 754 32-bit floating-point number.
- double: An IEEE 754 64-bit floating-point number.

**Non-Terminals:**
- `<natural>`
- `<integer>`
- `<int32>`
- `<int64>`
- `<uint32>`
- `<uint64>`
- `<float>`
- `<double>`
- `<integral>` --- for any integral type
- `<floating-point>` --- for any floating-point type
- `<number>` for any of the number types in this feature

**Operations:**
- `+, *, /, ^, -, %`: Standard arithmetic.
- `ceil, floor`: Useful float functions.

### String
**Import Name:** string

**Types:**
- string: An efficient, packed UTF-8 string.

**Non-Terminals:**
- `<digit>` --- Digits
- `<nondigit>` --- All utf-8 nondigit characters excluding newlines.
- `<utf-8-char>` --- All allowable utf-8 characters. 

**Keywords:**
This language feature implements no keywords. 

**Operations:**
- `+`: String concatenation
- `[]`: Glyph at a given position, well defined even if the position does not
  exist.
- `rev`: Reverse the string.
- `split`: Split a string with a separator to a list of strings (must have list
  imported).
- `join`: Join a list of strings with a separator (must have list imported).

### List
**Import Name:** list

**Types:**
- list: A doubly-linked list. Contains homogeneous data specified at compile 
  time.

**Non-Terminals:**
Defines no non-terminals.

**Operations:**
- `[]`: Constructs an empty list (with no args) or a list with items if given
  comma-separated values.
- `[]`: Addressing a list variable. Item at a given position, well defined even 
  if the position does not exist.
- `+`: List concatenation
- `:`: Cons

### Matrix
**Import Name:** matrix

**Types:**
- matrix: An n-dimensional fixed-size matrix

**Non-Terminals:**
Defines no non-terminals.

**Keywords:**
Defines no keywords. 

**Operations:**
- `+, *, /, ^, -`: Standard arithmetic.
- `||`: Constructs an empty matrix, or with items given comma and semicolon 
  separated values.
- `[]`: Addressing a matrix variable.
- `at`: Item at a given position, well defined even if the position does not
  exist.

### Traverse
**Import Name:** traverse

**Types:**
This feature imports no types.

**Non-Terminals:**
Defines no non-terminals.

**Operations:**
- `map`: Applies a function (not procedure) across a structure. Takes the 
  function and traversal arguments.
- `fold`: Applies a left fold using a given function and initial value over the
  provided structure.
- `filter`: Applies a predicate over the structure, retaining items for which
  the predicate returns true.

### Funcall
**Import Name:** funcall

**Types:**
This feature imports no types.

**Non-Terminals:**
Defines no non-terminals.

**Operations:**
- `defproc`: Define a procedure. Procedures may call functions
- `callproc`: Calls a procedure by name with given arguments.
- `deffun`: Define a function. Functions may not call functions or procedures.
- `callfun`: Call a function by name with given arguments.
