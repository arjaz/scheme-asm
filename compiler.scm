(define program '())
(define (emit instruction . args)
  (set! program (append program (list (apply format #f (string-append instruction "~%") args)))))

(define (compile-program x)
  (unless (integer? x) (error "only fixnum integers are supported"))
  (set! program '())
  (emit "    .text")
  (emit "    .globl scheme_entry")
  (emit "    .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "    movl $~a, %eax" x)
  (emit "    ret")
  program)

(define (write-program filename program)
  (let ((asm-source (string-join program "")))
    (call-with-output-file filename
      (lambda (port)
        (display asm-source port)
        (close-port port)))))

(write-program "target/scheme.s" (compile-program 18))
