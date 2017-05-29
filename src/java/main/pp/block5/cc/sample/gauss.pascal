Program gcd;

Var x, y, z: Integer;

Begin
    In("upper bound? ", x);
    y := 1
    z := 0
    While y < x Do
    Begin
        z := z + y
        y := y + 1
    End;
    Out("Sum of all integers up to (but excluding) ", x, ": ", z)
End.