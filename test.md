### Lambda : **Error**
- Get:
```lisp
yanis@DESKTOP-JR2E408:~/glados$ ./glados < coco.scm
List [Atom (String "lambda"),List [Atom (String "a"),Atom (String "b")],List [Atom (String "+"),Atom (String "a"),Atom (String "b")]]
Lambda
```
- Expected:
```lisp
~/B-FUN-500> cat lambda1.scm
(lambda (a b) (+ a b))
~/B-FUN-500> ./glados < lambda1.scm
#\<procedure\>
```

-----

cat superior.scm:
```
(define (> a b)
    (if (eq? a b)
        #f
        (if (< a b)
            #f
            #t)))
(> 10 -2)
```

- Got 
```
1:10:
unexpected '('
')'
  |
1 |  (define (> a b) (if (eq? a b) #f (if (< a b) #f #t)))
  |          ^
```

- Expected
```
#t
```

cat factorial.scm:
```
(define (fact x)
    (if (eq? x 1)
        1
        (* x (fact (- x 1)))))
(fact 10)
```

- Got
```
<stuck>
```

- Expected
```
3628800
```
