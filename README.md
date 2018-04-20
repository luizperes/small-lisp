# small-lisp
> A Small Lisp REPL fully written in Haskell (It is possible to run mostly anything in this interpreter, but some bugs are yet to be fixed).

### Documentation
You can find the EBNF and much more on __Small Lisp__ [here](http://www.cs.sfu.ca/~cameron/Teaching/384/chap2.ps).

### How to get started
Simply run: `ghc sl.hs interpreter.hs parser.hs tokenizer.hs unparser.hs`

#### Running
`./sl <filename>`

##### Example
`./sl test/third.sl`

###### REPL will start
```Haskell
REPL> { x = plus[2; 3] : times[4; x] }
20
```

_// Obs.: The REPL will not work if you do not pass at least one file as param._

### Hello World
###### hello.sl
```Haskell
hello-world[x] = cons[x;  (hello world)]
```

```Haskell
./sl hello.sl
REPL> hello-world["luiz"]
(luiz hello world)
```

### Brainfuck Interpreter
_// Please not that the BF interpreter is incomplete_
###### bf.sl
```Haskell
;;;
;;; Brainfuck
;;;

;;; Util functions

append[list1; list2] =
  [endp[list1] --> list2;
   otherwise --> cons[first[list1]; append[rest[list1]; list2]]]

length[list] =
  [ endp[list] --> 0
  ; otherwise --> plus[length[rest[list]]; 1] ]

element-at[idx; tape; accTape] =
  [ endp[tape] --> error["NoElementError"]
  ; otherwise --> {
      tail = [ endp[rest[tape]] --> (); otherwise --> rest[tape] ] :
      [ eqp[length[accTape]; idx] --> first[tape]
      ; otherwise --> element-at[idx; tail; append[accTape; cons[head;()]]] ]
  }]

setv-element-at[v; idx; tape; accTape] =
  [ endp[tape] --> accTape
  ; otherwise --> {
      tail = [ endp[rest[tape]] --> (); otherwise --> rest[tape] ] :
      [ eqp[length[accTape]; idx] --> setv-element-at[v; idx; tail; append[accTape; cons[v;()]]]
      ; otherwise --> setv-element-at[v; idx; tail; append[accTape; cons[first[tape];()]]] ]
  }]

add-value[v; idx; tape] =
  setv-element-at[plus[element-at[idx; tape; ()]; v]; idx; tape; ()]

;;; Brainfuck related functions

reset-tape[x; tape] =
  [ lessp[x; 0] --> tape
  ; otherwise --> reset-tape[minus[x; 1]; cons[0; tape]]]

inc[idx; tape] = add-value[1; idx; tape]

dec[idx; tape] = add-value[-1; idx; tape]

inc-ptr[idx] = plus[1; idx]

dec-ptr[idx] = minus[idx; 1]

exec[accCode; realCode; idx; tape; stack] =
  [ endp[accCode] --> tape
  ; otherwise --> {
      head = first[accCode];
      tail = [ endp[rest[accCode]] --> (); otherwise --> rest[accCode] ] :
      [ eq[+; head] --> exec[tail; realCode; idx; inc[idx; tape]; stack]
      ; eq[-; head] --> exec[tail; realCode; idx; dec[idx; tape]; stack]
      ; eq[>; head] --> exec[tail; realCode; inc-ptr[idx]; tape; stack]
      ; eq[<; head] --> exec[tail; realCode; dec-ptr[idx]; tape; stack]
      ; eq[?; head] --> exec[tail; realCode; dec-ptr[idx]; tape; stack]
      ; eq[|; head] --> exec[tail; realCode; dec-ptr[idx]; tape; stack] ]
  }]

bf[code; tape-size] = {
  arrCode = explode[code] :
  exec[arrCode; arrCode; 0; reset-tape[tape-size; ()]; ()]
}
```
