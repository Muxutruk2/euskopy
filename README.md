# BasquePy

Simple esolang that transpiles to python, written in grammatically correct Basque.

## Syntax

BasquePy only supports ints and floats as values

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
