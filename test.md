### Binding : **Error**
- Get:
```lisp
yanis@DESKTOP-JR2E408:~/glados$ ./glados
> foo
Atom (String "foo")
Just (foo = Nothing)
> (define foo 42)
List [Atom (String "define"),Atom (String "foo"),Atom (Number 42)]
Just (foo = Just 42)
> foo
Atom (String "foo")
Just (foo = Nothing)
>
```
- Expected:
```lisp
> foo
*** ERROR : variable foo is not bound.
> (define foo 42)
> foo
42
```

### Output : **Error**
Faut enlever les débug et les JUST
- Get:
```lisp
yanis@DESKTOP-JR2E408:~/glados$ ./glados
> (div 10 2)
List [Atom (String "div"),Atom (Number 10),Atom (Number 2)]
Just 5
>
```
- Expected:
```lisp
yanis@DESKTOP-JR2E408:~/glados$ ./glados
> (div 10 2)
5
```

### Lambda : **Error**
- Get:
```lisp
yanis@DESKTOP-JR2E408:~/glados$ ./glados < coco.scm
List [Atom (String "lambda"),List [Atom (String "a"),Atom (String "b")],List [Atom (String "+"),Atom (String "a"),Atom (String "b")]]
Just Lambda
```
- Expected:
```lisp
~/B-FUN-500> cat lambda1.scm
(lambda (a b) (+ a b))
~/B-FUN-500> ./glados < lambda1.scm
#\<procedure\>
```

### Function + Lambda : **Error**
En gros les fonction ne fonctionne pas
- Input
```lisp
cat test.scm

(define add
    (lambda (a b)
        (+ a b)))
(add 3 4)
```
- Get:
```lisp
yanis@DESKTOP-JR2E408:~/glados$ ./glados < coco.scm
List [Atom (String "define"),Atom (String "add"),List [Atom (String "lambda"),List [Atom (String "a"),Atom (String "b")],List [Atom (String "+"),Atom (String "a"),Atom (String "b")]]]
Just (add = Just Lambda)
List [Atom (String "add"),Atom (Number 3),Atom (Number 4)]
Nothing
```
- Expected:
```lisp
~/B-FUN-500> ./glados < lambda3.scm
7
```