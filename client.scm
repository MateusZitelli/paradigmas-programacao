(use tcp)
(require-extension srfi-13)

(define commands
  '("login mateus mateus"
    "compra PTBR 10 10"
    "compra EUA 15 15"
    "compra PTBR 3 3.33"
    "vende PTBR 5 25"
    "vende EUA 30 1.50"
    "vende EUA 25 45.50"
    "vende EUA 32 32.50"
    "vende PTBR 30 27.50"
    "vende PTBR 11 5.50"
    "ativas PTBR"
    "executadas PTBR"
    "ativas EUA"
    "executadas EUA"
    "catalogo"
    "lista PTBR"
    "lista EUA"
    "cotacao EUA"))

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
