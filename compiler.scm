(use-modules (srfi srfi-1))

(define noisy? #f)
(define program '())

(define (emit instruction . args)
  (set! program (append program (list (apply format #f (string-append instruction "~%") args)))))

(define (emit-comment msg . args)
  (when noisy?
    (apply emit (string-append "    # " msg) args)))

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format #f "L_~s" count)])
        (set! count (+ count 1))
        L))))

;; fixnums, booleans, characters and ()
;; immediate constant representation:
;; content + tag
;; fixnum tag: 0b00 in the two least significant bits, 30 bits of data
;; other constants: 0b1111 in the two least significatnt bits
;; #f: 0b00101111 - 2 bytes
;; #t: 0b01101111 - 2 bytes
;; character: #b00001111 tag, one byte to represent the value
;; (): #b00111111 - 2 bytes

(define fixnum-shift 2)
(define fixnum-mask #b11)
(define fixnum-tag #b00)
(define bool-f #b00101111)
(define bool-t #b01101111)
(define bool-mask #b10111111)
(define bool-tag #b00101111)
(define bool-bit 6) ; 6th bit determines whether it's #t or #f
(define char-shift 8)
(define char-mask #b11111111)
(define char-tag #b00001111)
(define empty-list #b00111111)
(define wordsize 4) ; bytes

(define fixnum-bits (- (* wordsize 8) fixnum-shift)) ; 30 bits

(define fixnum-min (- (expt 2 (- fixnum-bits 1))))
(define fixnum-max (- (expt 2 (- fixnum-bits 1)) 1))

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fixnum-min x fixnum-max)))

(define (immediate? x)
  (or (fixnum? x) (boolean? x) (char? x) (null? x)))

(define (immediate-rep x)
  (cond
   [(fixnum? x) (ash x fixnum-shift)]
   [(eq? x #t)  bool-t]
   [(eq? x #f)  bool-f]
   [(char? x)   (logior (ash (char->integer x) char-shift) char-tag)]
   [(null? x)   empty-list]
   [else        (error "not an immediate constant")]))

(define (emit-immediate x)
  (emit-comment "imm: ~s, 0b~a" x (number->string (immediate-rep x) 2))
  (emit "    movl $~s, %eax" (immediate-rep x)))

;; Primitives are stored in the primitives-alist in the following form:
;; (primitive-name . (argument-count emitter-lambda))
(define primitives-alist '())
(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name si env arg* ...) b b* ...)
     (set! primitives-alist
           (assoc-set! primitives-alist 'prim-name
                       (list (length '(arg* ...))
                             (lambda (si env arg* ...) b b* ...))))]))

(define (primitive? x)
  (and (symbol? x) (assq x primitives-alist) #t))

(define (primitive-emitter x)
  (let ((primitive-info (assq x primitives-alist)))
    (if primitive-info
        (caddr primitive-info)
        (error "not a primitive"))))

(define (primitive-args-number x)
  (let ((primitive-info (assq x primitives-alist)))
    (if primitive-info
        (cadr primitive-info)
        (error "not a primitive"))))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

;; for now just checks the number of arguments
(define (check-primcall-args prim args)
  (unless (= (length args) (primitive-args-number prim))
    (error "wrong number of arguments")))

(define (emit-primcall si env expr)
  (let ([prim (car expr)]
        [args (cdr expr)])
    (check-primcall-args prim args)
    (emit-comment "prim: ~a" prim)
    (apply (primitive-emitter prim) si env args)
    (emit-comment "^ prim: ~a" prim)))

(define (emit-cmp->bool)
  ;; set the lower 16 bits of the result (%al) to 1 if cmp is true
  ;; can't set all 32 bits, but only the lower 16 bits
  (emit "    sete %al")
  ;; extend the sign of %al to %eax, both 0 and 1 extend with 0s
  (emit "    movzbl %al, %eax")
  ;; shift left to the relevant boolean bit
  (emit "    sal $~s, %al" bool-bit)
  ;; apply the boolean tag
  (emit "    or $~s, %al" bool-tag))

;; unary primitives:
;; fxadd1
;; fxsub1
;; char->fixnum
;; fixnum->char
;; fxzero?
;; null?
;; not
;; fixnum?
;; boolean?
;; char?
;; fxlognot

(define-primitive (fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "    addl $~s, %eax" (immediate-rep 1)))

(define-primitive (fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "    subl $~s, %eax" (immediate-rep 1)))

(define-primitive (char->fixnum si env arg)
  (emit-expr si env arg)
  (emit "    shrl $~s, %eax" (- char-shift fixnum-shift)))

(define-primitive (fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "    shll $~s, %eax" (- char-shift fixnum-shift))
  (emit "    orl $~s, %eax" char-tag))

(define-primitive (fxzero? si env arg)
  (emit-expr si env arg)
  (emit "    cmpl $~s, %eax" (immediate-rep 0))
  (emit-cmp->bool))

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit "    cmpl $~s, %eax" empty-list)
  (emit-cmp->bool))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  ;; compare to #f
  (emit "    cmpl $~s, %eax" bool-f)
  (emit-cmp->bool))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit "    and $~s, %al" fixnum-mask)
  (emit "    cmp $~s, %al" fixnum-tag)
  (emit-cmp->bool))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit "    andl $~s, %eax" bool-mask)
  (emit "    cmpl $~s, %eax" bool-tag)
  (emit-cmp->bool))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit "    andl $~s, %eax" char-mask)
  (emit "    cmpl $~s, %eax" char-tag)
  (emit-cmp->bool))

(define-primitive (fxlognot si env arg)
  (emit-expr si env arg)
  (emit "    xorl $~s, %eax" (immediate-rep fixnum-min)))

;; binary primitives:
;; fx+
;; fx-
;; fxlogand
;; fxlogor
;; fx=
;; TODO:
;; fx<
;; fx<=
;; fx>
;; fx>=
;; char=
;; char<
;; char<=
;; char>
;; char>=

;; TODO: generic emit-mov
(define (emit-mov-eax-stack si)
  (emit "    movl %eax, ~s(%esp)" si))

(define (emit-mov-stack-eax si)
  (emit "    movl ~s(%esp), %eax" si))

(define (next-stack-index si)
  (- si wordsize))

(define-primitive (fx+ si env arg1 arg2)
  ;; evaluate the left argument
  (emit-expr si env arg1)
  ;; store the result at the current stack index
  (emit-mov-eax-stack si)
  ;; evaluate the right argument, adjusting the stack index
  (emit-expr (next-stack-index si) env arg2)
  (emit "    addl ~s(%esp), %eax" si))

(define-primitive (fx- si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-mov-eax-stack si)
  (emit-expr (next-stack-index si) env arg2)
  (emit-mov-eax-stack (next-stack-index si))
  (emit-mov-stack-eax si)
  (emit "    subl ~s(%esp), %eax" (next-stack-index si)))

(define-primitive (fxlogand si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-mov-eax-stack si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    andl ~s(%esp), %eax" si))

(define-primitive (fxlogor si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-mov-eax-stack si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    or ~s(%esp), %eax" si))

(define-primitive (fx= si env arg1 arg2)
  (emit-expr si env arg1)
  (emit-mov-eax-stack si)
  (emit-expr (next-stack-index si) env arg2)
  (emit "    cmpl ~s(%esp), %eax" si)
  (emit-cmp->bool))

;; (if test then else)
(define (if? expr)
  (and (pair? expr)
       (eq? (car expr) 'if)
       (eq? (length expr) 4)))

(define (if-test expr)
  (cadr expr))

(define (if-then expr)
  (caddr expr))

(define (if-else expr)
  (cadddr expr))

;; TODO: and, or
(define (emit-if si env expr)
  (let ([else-label (unique-label)]
        [end-label (unique-label)])
    (emit-comment "if test")
    (emit-expr si env (if-test expr))
    (emit-comment "^ if test")
    (emit "    cmp $~s, %al" bool-f)
    (emit "    je ~a" else-label)
    (emit-comment "then")
    (emit-expr si env (if-then expr))
    (emit "    jmp ~a" end-label)
    (emit-comment "^ then")
    (emit-comment "else")
    (emit "~a:" else-label)
    (emit-expr si env (if-else expr))
    (emit-comment "^ else")
    (emit "~a:" end-label)
    (emit-comment "^ if")))

;; (let ((bind1 expr1) ...) body)
(define (let? expr)
  (and (pair? expr)
       (eq? (car expr) 'let)
       (every (lambda (binding) (eq? (length binding) 2)) (let-bindings expr))
       #t))

(define (let-bindings expr)
  (cadr expr))

(define (let-body expr)
  (caddr expr))

(define (lhs let-binding)
  (car let-binding))

(define (rhs let-binding)
  (cadr let-binding))

(define (extend-env name si env)
  (acons name si env))

;; TODO: let*
;; env is an alist of form ((var-name . stack-offset)...)
(define (emit-let si env expr)
  (define (process-let bindings si new-env)
    (cond
     [(null? bindings)
      (begin
        (emit-comment "let expr body")
        (emit-expr si new-env (let-body expr))
        (emit-comment "^ let expr body"))]
     [else
      (let ([b (car bindings)])
        (emit-comment "let rhs (~s)" (lhs b))
        (emit-expr si env (rhs b))
        (emit-mov-eax-stack si)
        (emit-comment "^ let rhs (~s)" (lhs b))
        (process-let (cdr bindings)
                     (next-stack-index si)
                     (extend-env (lhs b) si new-env)))]))
  (process-let (let-bindings expr) si env))

;; letrec is used for toplevel function bindings

;; moves the %esp register by si points
;; should be used to point the stack at the last
;; local variable before the function call
;; (so `call` would adjust the %esp register by 4)
;; or return the %esp register to its previous value
;; after the call
(define (emit-adjust-base si)
  (if (>= si 0)
      (emit "    addl $~s, %esp" si)
      (emit "    subl $~s, %esp" (- si))))

(define (emit-call si f)
  (emit "    call ~a" f))

;; (app lvar expr ...)
(define (call-target expr)
  (cadr expr))

(define (call-args expr)
  (cddr expr))

(define (app? expr)
  (eq? (car expr) 'app))

;; application evaluates all function arguments
;; and places them on the stack
;; reserving one empty word between the current function locals (the current si)
;; and the evaluated parameters to place the return address there (via `call`)
(define (emit-app si env expr)
  (define (emit-arguments si args)
    (unless (null? args)
      (emit-expr si env (car args))
      (emit-mov-eax-stack si)
      (emit-arguments (- si wordsize) (cdr args))))
  ;; (- si wordsize) to reserve a slot
  (emit-arguments (- si wordsize) (call-args expr))
  ;; (+ si wordsize) as si points to the next empty slot
  (emit-adjust-base (+ si wordsize))
  (emit-call si (cdr (assoc (call-target expr) env)))
  (emit-adjust-base (- (+ si wordsize))))

;; (lambda (arg1 ...) body)
(define (lambda-formals expr)
  (cadr expr))

(define (lambda-body expr)
  (caddr expr))

(define (emit-lambda env)
  (lambda (expr label)
    (emit-function-header label)
    (let ([fmls (lambda-formals expr)]
          [body (lambda-body expr)])
      (let f ([fmls fmls]
              [si   (- wordsize)]
              [env  env])
        (cond
         [(null? fmls)
          (begin
            (emit-expr si env body)
            (emit "    ret"))]
         [else
          (f (cdr fmls)
             (- si wordsize)
             (extend-env (first fmls) si env))])))))

;; make a list of unique labels for each of the variable names
(define (unique-labels lvars)
  (map (lambda (lvar) (string-append (unique-label) "_" (symbol->string lvar))) lvars))

;; (letrec ((lvar <lambda>) ...) body)
(define (letrec? expr)
  (eq? (car expr) 'letrec))

(define (letrec-bindings expr)
  (cadr expr))

(define (letrec-body expr)
  (caddr expr))

(define (make-initial-env lvals labels)
  (map (lambda (lval label) (cons lval label)) lvals labels))

(define (emit-letrec expr)
  (let* ([bindings (letrec-bindings expr)]
         [lvars    (map lhs bindings)]
         [lambdas  (map rhs bindings)]
         [labels   (unique-labels lvars)]
         [env      (make-initial-env lvars labels)])
    (for-each (emit-lambda env) lambdas labels)
    (emit-scheme-entry (letrec-body expr) env)))

;; variables are stored on the stack
(define (emit-variable-ref env var)
  (let ((var-pair (assoc var env)))
   (cond
   [var-pair (emit-mov-stack-eax (cdr var-pair))]
   [else     (error "undefined" var)])))

(define (var? expr)
  (symbol? expr))

(define (emit-expr si env expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(if? expr)        (emit-if si env expr)]
   [(primcall? expr)  (emit-primcall si env expr)]
   [(let? expr)       (emit-let si env expr)]
   [(var? expr)       (emit-variable-ref env expr)]
   [(app? expr)       (emit-app si env expr)]
   [else              (error "expression not supported" expr)]))

(define (emit-function-header function-name)
  (emit "")
  (emit "    .globl ~a" function-name)
  (emit "    .type ~a, @function" function-name)
  (emit "~a:" function-name))

(define (emit-prelude)
  (emit "    .text"))

(define (emit-scheme-entry expr env)
  (emit-function-header "L_scheme_entry")
  (emit-expr (- wordsize) env expr)
  (emit "    ret")
  (emit-function-header "scheme_entry")
  (emit "    movl %esp, %ecx")
  (emit "    movl 4(%esp), %esp")
  (emit "    call L_scheme_entry")
  (emit "    movl %ecx, %esp")
  (emit "    ret"))

(define (emit-program expr)
  (emit-prelude)
  (cond
   [(letrec? expr) (emit-letrec expr)]
   [else           (emit-scheme-entry expr '())]))

(define (compile-program expr)
  (set! program '())
  (emit-program expr)
  program)

(define (write-program filename program)
  (let ((asm-source (string-join program "")))
    (call-with-output-file filename
      (lambda (port) (display asm-source port)))))

(write-program
 "target/scheme.s"
 (compile-program
  '(letrec ((left (lambda (x y) x))) (app left -10 +10))))
