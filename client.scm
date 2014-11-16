(use tcp)
(require-extension srfi-13)

(define commands
  '("login mateus mateus"
    "compra PTBR 10 10"
    "compra PTBR 15 15"
    "vende PTBR 1.50 30"
    "ativas PTBR"
    "catalogo"
    ))

(let run-commands ((commands commands))
 (if (null? commands)
   '()
   (let-values (((in out) (tcp-connect "127.0.0.1" 9009)))
               (display "< ")
               (display (car commands))
               (newline)
               (write-line (car commands) out)
               (map display
                    (map 
                      (lambda (s) 
                        (string-concatenate (list "> " s "\n")))
                      (read-lines in)))
               (newline)
               (run-commands (cdr commands)))))
