(define program '())
(define (emit instruction . args)
  (set! program (append program (list (apply format #f (string-append instruction "~%") args)))))

;; fixnums, booleans, characters and ()
;; immediate constant representation:
;; tag + content
;; fixnum tag: 0b00 in the two least significant bits, 30 bits of data
;; other constants: 0b1111 in the two least significatnt bits
;; #f: 0b00101111 - 2 bytes
;; #t: 0b01101111 - 2 bytes
;; character: #b00001111 tag, one byte to represent the value
;; (): #b00111111 - 2 bytes

(define fixnum-shift 2)
(define fixnum-mask #b11)
(define fixnum-tag #b00)
(define bool-t #b00101111)
(define bool-f #b01101111)
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
   [(char? x)   (logior (ash (char->integer x) char-shift) char-mask)]
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
    [(_ (prim-name arg* ...) b b* ...)
     (set! primitives-alist
           (assoc-set! primitives-alist 'prim-name
                       (list (length '(arg* ...))
                             (lambda (arg* ...) b b* ...))))]))

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
  (or (= (length args) (primitive-args-number prim))
      (error "wrong number of arguments")))

(define (emit-primcall expr)
  (let ([prim (car expr)]
        [args (cdr expr)])
    (check-primcall-args prim args)
    (emit "    # prim: ~a" prim)
    (apply (primitive-emitter prim) args)
    (emit "    # ^ prim: ~a" prim)))

;; TODO: unary primitives:
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

(define-primitive (fxadd1 arg)
  (emit-expr arg)
  (emit "    addl $~s, %eax" (immediate-rep 1)))

(define-primitive (fxsub1 arg)
  (emit-expr arg)
  (emit "    subl $~s, %eax" (immediate-rep 1)))

(define-primitive (char->fixnum arg)
  (emit-expr arg)
  (emit "    shrl $~s, %eax" (- char-shift fixnum-shift)))

(define-primitive (fixnum->char arg)
  (emit-expr arg)
  (emit "    shll $~s, %eax" (- char-shift fixnum-shift))
  (emit "    orl $~s, %eax" char-tag))

(define (emit-expr expr)
  (cond
   [(immediate? expr) (emit-immediate expr)]
   [(primcall? expr)  (emit-primcall expr)]
   [else              (error "expression not supported")]))

(define (emit-function-header function-name)
  (emit "    .text")
  (emit "    .globl ~a" function-name)
  (emit "    .type ~a, @function" function-name)
  (emit "~a:" function-name))

(define (emit-program expr)
  (emit-function-header "scheme_entry")
  (emit-expr expr)
  (emit "    ret"))

(define (compile-program expr)
  (set! program '())
  (emit-program expr)
  program)

(define (write-program filename program)
  (let ((asm-source (string-join program "")))
    (call-with-output-file filename
      (lambda (port)
        (display asm-source port)
        (close-port port)))))

(write-program "target/scheme.s" (compile-program '(fixnum->char (fxadd1 (fxsub1 65)))))
