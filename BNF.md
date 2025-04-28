# Zoisite Language BNF

## Program Structure

```bnf
<root> ::= <statement>* <eof>
```

## Statements

```bnf
<statement> ::= <let_statement>
              | <while_statement>
              | <break_statement>
              | <continue_statement>
              | <expression_statement>
              | <empty_statement>
              | <function_definition>

<let_statement> ::= "let" ( <identifier> | <typed_identifier> ) "=" <expression> ";"

<while_statement> ::= "while" "(" <expression> ")" <expression>

<break_statement> ::= "break" ";"

<continue_statement> ::= "continue" ";"

<expression_statement> ::= <expression> ";"

<empty_statement> ::= ";"
```

## Function Definition

```bnf
<function_definition> ::= "fun" <identifier> <parameter_list> ":" <type_specifier> <block_expression>

<parameter_list> ::= "(" [ <typed_identifier> ( "," <typed_identifier> )* ] ")"

<typed_identifier> ::= <identifier> ":" <type_specifier>
```

## Expressions

```bnf
<expression> ::= <assignment_expr> | <logical_expr>

<assignment_expr> ::= <lvalue> "=" <expression>

<lvalue> ::= <identifier> | <lvalue> <index_suffix>

<logical_expr> ::= <comparison_expr> ( ( "&&" | "||" ) <comparison_expr> )*

<comparison_expr> ::= <additive_expr> ( ( "==" | "!=" | "<" | ">" | "<=" | ">=" ) <additive_expr> )*

<additive_expr> ::= <multiplicative_expr> ( ( "+" | "-" ) <multiplicative_expr> )*

<multiplicative_expr> ::= <unary_expr> ( ( "*" | "/" | "%" ) <unary_expr> )*

<unary_expr> ::= <prefix_expr> | <postfix_expr>

<prefix_expr> ::= "-" <unary_expr>

<postfix_expr> ::= <primary_expr> ( <call_suffix> | <index_suffix> )*

<call_suffix> ::= "(" [ <argument_list> ] ")"

<index_suffix> ::= "[" <expression> "]"

<argument_list> ::= <expression> ( "," <expression> )*

<primary_expr> ::= <literal>
                 | <identifier>
                 | <paren_or_tuple_expr>
                 | <block_expr>
                 | <array_literal>
                 | <if_expr>

<paren_expr> ::= "(" <expression> ")"

<tuple_expr> ::= "(" <expression> ( "," <expression> )+ ")"

<block_expr> ::= "{" <statement>* [ <expression> ] "}"

<if_expr> ::= "if" "(" <expression> ")" <expression> [ "else" <expression> ]

<array_literal> ::= "[" <expression> ";" <expression> "]"
```

## Literals

```bnf
<literal> ::= <integer_literal>
            | <float_literal>
            | <string_literal>
            | <char_literal>
            | <boolean_literal>
            | <none_literal>

<integer_literal> ::= <digit>+
<float_literal> ::= <digit>+ "." <digit>*
<string_literal> ::= '"' ( <character> )* '"'
<char_literal> ::= "'" ( <character> ) "'"
<boolean_literal> ::= "true" | "false"
<none_literal> ::= "none"
```

## Type Specifiers

```bnf
<type_specifier> ::= <base_type_spec> ( <type_modifier> )*

<base_type_spec> ::= <ident_type_spec> | <tuple_type_spec>

<ident_type_spec> ::= <identifier>

<tuple_type_spec> ::= "(" <type_specifier> ( "," <type_specifier> )+ ")"

<type_modifier> ::= "[" "]" | "?"
```

## Lexical Elements

```bnf
<identifier> ::= <letter> ( <letter> | <digit> | "_" )*
<letter> ::= "a"..."z" | "A"..."Z"
<digit> ::= "0"..."9"
<character> ::= /* any valid character within strings/chars */
<eof> ::= /* end of input */
```

## Comments

```bnf
<comment> ::= "//" /* any sequence of characters except newline */ <newline>
```
