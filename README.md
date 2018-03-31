# RecursiveDescentCompiler

This is a recursive descent compiler for the following grammar:

```
<program>     ->  begin <stmt_list> end
<stmt>        ->  <id> := <expr> | ε
<stmt_list>   ->  <stmt_list> ; <stmt> | <stmt>
<expr>        ->  <expr> + <term> | <expr> - <term> |  <term>
<term>        ->  <term> * <factor> | <term> div <factor> | <term> mod <factor> |  <factor>
<factor>      ->  <primary> ^ <factor> | <primary>
<primary>     ->  <id> | <num> | ( <expr> )
```

An example input program would be:

```
begin
    alpha := 20; gamma := 11; C3P0 := 5; R2D2 := 4; 		 	
    answer := alpha + 2 * gamma div (C3P0 - R2D2);
    gun := (i mod ammo)^2^2^2
end 
```

And the output for this example would be:

```
LVALUE alpha
PUSH 20
STO
LVALUE gamma
PUSH 11
STO
LVALUE C3P0
PUSH 5
STO
LVALUE R2D2
PUSH 4
STO
LVALUE answer
RVALUE alpha
PUSH 2
RVALUE gamma
MPY
RVALUE C3P0
RVALUE R2D2
SUB
DIV
ADD
STO
LVALUE gun
RVALUE i
RVALUE ammo
MOD
PUSH 2
PUSH 2
PUSH 2
POW
POW
POW
STO
HALT
Compilation completed
```
