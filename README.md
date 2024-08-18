# Scripty language compiler

## Idea and language Goal

This project is for learning purpose. I started it to apply some techniques used in compilers from parsing source code to code generation.

The goal is to build a simple high level language. The code will be transpiled to C and maybe to a real hardware architecture.

My inspiration came mainly from three youtube videos

- [Building a compiler in rust by Julian Hartl](<https://www.youtube.com/watch?v=GAU51Dqsp3Y&list=PLI1h1vRqlHLNZAa2BEM9uZ2GEvUNYDasO>) (The code is highly impacted by this playlist)

- [coding a compiler in Rust- Loki by AmirrezaAsk](<https://www.youtube.com/watch?v=qHNLqkyJnX8&list=PLS87DlLl8etxauk3Gs184H8nGaekKsj4O>)

- [DIY Programming Language #1: The Shunting Yard Algorithm by javidx9](<https://www.youtube.com/watch?v=unh6aK8WMwM&pp=ygUHamF2aWR4OQ%3D%3D>) (This algorithm is used to build expressions in the compiler)

## Compiler components

- Lexer: Transform the source code text to a stream of Tokens
- Parser: Transform Tokens to Statements and Expressions to build an Abstract Syntax Tree
- Resolver: Perform semantic check on the code (check declared variables, typing, etc)
- Evaluator: will evaluate the code and returns the values of the last expression (used for testing)

### Coming

- Optimizer: Perform optimizations

## Language regex grammar

This is a (high level) description of the language (in it's current state)

```Grammar
statement := let_statement | expression_statement

let_statement := let identifier = expression

expression_statement := expression ; | expression

identifier := character | character digits | character identifier

character := [A-Za-z_]
```

## Error reporting

Errors are gathered into a list of Diagnostics which can be an Error or a Warning (to be added in the future)

Errors are checked after each step. If the parser finds any syntax errors the compilation process stops. After that we pass the Ast to the resolver to gather semantic errors

## Examples

```Scripty
let x = 4
let y = x = 5
let c = true && (false || true)
let z = 4 * 5 / 20
```

The code above declare 4 variables x, y, c, z with `let` statement. x, y and z have int type. variable c has bool type.

At the moment only int (64-bit signed integer) and boolean types are supported. Other types like strings will be added later. Also there is a plan to add custom types.

Semicolon is used as a separator between expressions. But it is not necessary, the compiler will insert it automatically when it thinks that an expression has finished. In other word when the compiler cannot add the next token into an expression, it will be considered as finished.
As an example the following code will be correct:

```Scripty
let x = 4;
let y = 7 x = x + y
```

The parser cannot put the token `x` after the `7` in the same expression. In this case it starts parsing a new expression. So it will be equivalent to this code:

```Scripty
let x = 4;
let y = 7;
x = x + y;
```

This behaviour may change in the future.

## Progress

- [x] Declaring variables
- [x] Arithmetic operations
- [x] Boolean logic operations
- [x] Adding type to expressions
- [x] Type checking
- [ ] Adding comparaison operators
- [ ] Adding block statement
- [ ] if-else statement
- [ ] while statement
