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
(define bool-t #b00101111)
(define bool-f #b01101111)
(define char-shift 8)
(define char-mask #b00001111)
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

(define (compile-program x)
  (unless (immediate? x) (error "only immediate constant are supported"))
  (set! program '())
  (emit "    .text")
  (emit "    .globl scheme_entry")
  (emit "    .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "    # ~s, binary: 0b~a" x (number->string (immediate-rep x) 2))
  (emit "    movl $~a, %eax" (immediate-rep x))
  (emit "    ret")
  program)

(define (write-program filename program)
  (let ((asm-source (string-join program "")))
    (call-with-output-file filename
      (lambda (port)
        (display asm-source port)
        (close-port port)))))

(write-program "target/scheme.s" (compile-program '()))
