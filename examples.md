### Typechecking errors

```
Discard(x) \ ()
```

```
H(q1).(Discard(q1) || Discard(q1)) \ ()
```

```
Discard(q1) ++ Discard(q2) \ ()
```

```
c:int!5 || c:int?x.Discard() \ (c: bool)
```

```
c:quant?x.(if x = 0 then o:quant!x else o:quant!x) \ ()
```

### Quantum Lottery

```
H(q1).M(q1 > x).((if x = 0 then a:int!1 else b:int!1) || Discard(q1)) \ ()
```

### Quantum teleportation protocol

```
H(q2).CX(q2,q3).((CX(q1,q2).H(q1).M(q1,q2 > n).(m:int!n || Discard(q1,q2))) || (m:int?n2.(if n2 = 0 then (I(q3).o:quant!q3) else (if n2 = 1 then (X(q3).o:quant!q3) else (if n2 = 2 then (Z(q3).o:quant!q3) else (Z(q3).X(q3).o:quant!q3)))))) \ (m:int)
```
