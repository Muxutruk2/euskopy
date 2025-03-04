# BasquePy

Simple esolang that transpiles to python, written in grammatically correct
Basque.

## Syntax

BasquePy only supports ints and floats as values

### Function Calls

Since Basque is a SOV language, the verb (function name) should go last. The
only function available is print (inprimatu)

```
(x)inprimatu
```

### Expressions

Expressions are formed with 2 operands and one operator, like * / + and so on.
Comparators are also expressions.

### Assignments

assignments with or without arithemtic

```
[VAR] [VALUE] (OP VALUE) dela ezarri;
```

For example:

```
n 29 dela ezarri;
lehena 1 + 5 dela ezarri;
```

### For Loops

BasquePy only supports for loops with range

```
[VAR] bakoitzeko [VALUE/VARIABLE] eta [VALUE/VARIABLE] artean {
    [BODY]
}
```

For example:

```
i bakoitzeko 1 eta 52 artean {

}
i bakoitzeko 0 eta x artean {

}
```

### If Statements

If Statements require a comparison which must have a left side that can be
either a variable, a number or an expression, then, a comparator like `==` and
the right side which like the left side.

```
x == 10 bada {
    (x)inprimatu
}
x + 5 == 15 bada {
    (x)inprimatu
}
x + 5 == 15 + 5 bada {
    (x)inprimatu
} bestela {
    (x)inprimatu
}
```

The else block goes like the example. Elif statement not supported but can
easily be avoided.
