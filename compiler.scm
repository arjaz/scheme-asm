(define program '())
(define (emit instruction . args)
  (let ((noisy #f))
    (when (or noisy (not (string-prefix? "#" (string-trim instruction))))
        (set! program (append program (list (apply format #f (string-append instruction "~%") args)))))))

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
  (emit "    # imm: ~s, 0b~a" x (number->string (immediate-rep x) 2))
  (emit "    movl $~s, %eax" (immediate-rep x)))

;; Primitives are stored in the primitives-alist in the following form:
;; (primitive-name . (argument-count emitter-lambda))
(define primitives-alist '())
(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name si arg* ...) b b* ...)
     (set! primitives-alist
           (assoc-set! primitives-alist 'prim-name
                       (list (length '(arg* ...))
                             (lambda (si arg* ...) b b* ...))))]))

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

(define (emit-primcall si expr)
  (let ([prim (car expr)]
        [args (cdr expr)])
    (check-primcall-args prim args)
    (emit "    # prim: ~a" prim)
    (apply (primitive-emitter prim) si args)
    (emit "    # ^ prim: ~a" prim)))

(define (emit-compare-to-bool)
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

(define-primitive (fxadd1 si arg)
  (emit-expr si arg)
  (emit "    addl $~s, %eax" (immediate-rep 1)))

(define-primitive (fxsub1 si arg)
  (emit-expr si arg)
  (emit "    subl $~s, %eax" (immediate-rep 1)))

(define-primitive (char->fixnum si arg)
  (emit-expr si arg)
  (emit "    shrl $~s, %eax" (- char-shift fixnum-shift)))

(define-primitive (fixnum->char si arg)
  (emit-expr si arg)
  (emit "    shll $~s, %eax" (- char-shift fixnum-shift))
  (emit "    orl $~s, %eax" char-tag))

(define-primitive (fxzero? si arg)
  (emit-expr si arg)
  (emit "    cmpl $~s, %eax" (immediate-rep 0))
  (emit-compare-to-bool))

(define-primitive (null? si arg)
  (emit-expr si arg)
  (emit "    cmpl $~s, %eax" empty-list)
  (emit-compare-to-bool))

(define-primitive (not si arg)
  (emit-expr si arg)
  ;; compare to #f
  (emit "    cmpl $~s, %eax" bool-f)
  (emit-compare-to-bool))

(define-primitive (fixnum? si arg)
  (emit-expr si arg)
  (emit "    and $~s, %al" fixnum-mask)
  (emit "    cmp $~s, %al" fixnum-tag)
  (emit-compare-to-bool))

(define-primitive (boolean? si arg)
  (emit-expr si arg)
  (emit "    andl $~s, %eax" bool-mask)
  (emit "    cmpl $~s, %eax" bool-tag)
  (emit-compare-to-bool))

(define-primitive (char? si arg)
  (emit-expr si arg)
  (emit "    andl $~s, %eax" char-mask)
  (emit "    cmpl $~s, %eax" char-tag)
  (emit-compare-to-bool))

(define-primitive (fxlognot si arg)
  (emit-expr si arg)
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

(define (emit-move-result-to-stack si)
  (emit "    movl %eax, ~s(%esp)" si))

;; TODO: make all of them accept more than 2 arguments
(define-primitive (fx+ si arg1 arg2)
  ;; evaluate the left argument
  (emit-expr si arg1)
  ;; store the result at the current stack index
  (emit-move-result-to-stack si)
  ;; evaluate the right argument, adjusting the stack index
  (emit-expr (- si wordsize) arg2)
  (emit "    addl ~s(%esp), %eax" si))

(define-primitive (fx- si arg1 arg2)
  (emit-expr si arg1)
  (emit-move-result-to-stack si)
  (emit-expr (- si wordsize) arg2)
  (emit-move-result-to-stack (- si wordsize))
  (emit "    movl ~s(%esp), %eax" si)
  (emit "    subl ~s(%esp), %eax" (- si wordsize)))

(define-primitive (fxlogand si arg1 arg2)
  (emit-expr si arg1)
  (emit-move-result-to-stack si)
  (emit-expr (- si wordsize) arg2)
  (emit "    andl ~s(%esp), %eax" si))

(define-primitive (fxlogor si arg1 arg2)
  (emit-expr si arg1)
  (emit-move-result-to-stack si)
  (emit-expr (- si wordsize) arg2)
  (emit "    or ~s(%esp), %eax" si))

(define-primitive (fx= si arg1 arg2)
  (emit-expr si arg1)
  (emit-move-result-to-stack si)
  (emit-expr (- si wordsize) arg2)
  (emit "    cmpl ~s(%esp), %eax" si)
  (emit-compare-to-bool))

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
(define (emit-if si expr)
  (let ([else-label (unique-label)]
        [end-label (unique-label)])
    (emit "    # if test")
    (emit-expr si (if-test expr))
    (emit "    # ^ if test")
    (emit "    cmp $~s, %al" bool-f)
    (emit "    je ~a" else-label)
    (emit "    # then")
    (emit-expr si (if-then expr))
    (emit "    jmp ~a" end-label)
    (emit "    # ^ then")
    (emit "    # else")
    (emit "~a:" else-label)
    (emit-expr si (if-else expr))
    (emit "    # ^ else")
    (emit "~a:" end-label)
    (emit "    # ^ if")))

(define (emit-expr si expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(if? expr)        (emit-if si expr)]
   [(primcall? expr)  (emit-primcall si expr)]
   [else              (error "expression not supported")]))

(define (emit-function-header function-name)
  (emit "    .globl ~a" function-name)
  (emit "    .type ~a, @function" function-name)
  (emit "")
  (emit "~a:" function-name))

(define (emit-prelude)
  (emit "    .text"))

(define (emit-program expr)
  (emit-prelude)
  (emit-function-header "L_scheme_entry")
  (emit-expr (- wordsize) expr)
  (emit "    ret")
  (emit "")
  (emit-function-header "scheme_entry")
  (emit "    movl %esp, %ecx")
  (emit "    movl 4(%esp), %esp")
  (emit "    call L_scheme_entry")
  (emit "    movl %ecx, %esp")
  (emit "    ret")
  (emit ""))

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
 (compile-program '(fx= (fx+ 10 -10) (fxadd1 -1))))
