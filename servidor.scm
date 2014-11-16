(use crypt) ; For save passwords
(use srfi-1)
(use srfi-69) ; Implements hash-tables
(use tcp srfi-18)

(define port 9009)

; --- Errors ----
(define invalid-user "001")
(define order-dont-exists "004")

; --- Alias ---
(define hts! hash-table-set!)
(define htr hash-table-ref)
(define htr/default hash-table-ref/default)

; --- Utils ---
(define (ht-navigate/create hash path-list)
  (let enter ((h hash)
              (l path-list))
    (if (null? l)
      h
      (let ((next-key (car l)))
       (if (hash-table-exists? h next-key)
         (enter (htr h next-key) (cdr l))
         (begin
           (hts! h next-key (make-hash-table))
           (enter (htr h next-key) (cdr l))))))))

(define (take l n)
  (if (or (null? l) (= n 0))
    (list)
    (cons (car l)
          (take (cdr l) (- n 1)))))

; --- File handling functions ---
(define (save-data configs file-name)
  (let ((out (open-output-file file-name)))
   (write configs out)
   (close-output-port out)))

(define (load-data file-name)
  (let ((in (open-input-file file-name)))
   (let ((data (read in)))
    (close-input-port in)
    data)))

; TODO: Optimizate to use tail call
(define (neasted-hash-table->neasted-alist h)
  (let ((alist (hash-table->alist h)))
   (let iterate ((l alist))
    (if (= (length l) 0)
      (list)
      (begin
        (if (hash-table? (cdar l))
          (set-cdr! (car l) (neasted-hash-table->neasted-alist (cdar l))))
        (cons (car l) (iterate (cdr l))))))))

; TODO: Implement with tail call
(define (neasted-alist->neasted-hash-table alist)
  (if (or (null? alist) (number? (car alist)))
    alist
    (let ((h (alist->hash-table alist)))
     (let iterator ((keys (hash-table-keys h)))
      (if (= (length keys) 0)
        (list)
        (let ((key (car keys))
              (value (htr h (car keys))))
          (if (list? value)
            (if (and (> (length value) 2) (not (list? (cadr value))))
              (hts! h key value)
              (hts! h key (neasted-alist->neasted-hash-table value))))
          (iterator (cdr keys))
          h))))))

(define (save-hash-table hash file-name)
  (save-data (neasted-hash-table->neasted-alist hash) file-name))

(define (load-hash-table file-name)
  (neasted-alist->neasted-hash-table (load-data file-name)))

(define (restore-data file-name)
  (if (file-exists? file-name)
    (load-hash-table file-name)
    (make-hash-table)))

(define (restore-db)
  ; not working yet(restore-data "main.dbscm")
  (make-hash-table))

(define (save-db db) (save-hash-table db "main.dbscm"))

(define (split-lines-from-port port)
  (let read-next-line ()
   (let ((line (read-line port)))
    (if (eof-object? line)
      (list)
      (let ((action (string-split line)))
       (cons action (read-next-line)))))))

; Return a list of the splited lines contened in a file
(define (load-lines actions-file)
  (call-with-input-file actions-file split-lines-from-port))

; --- Server responses ---
(define (send-message message out)
  (display message out)
  (newline out))

(define (send-error error-code out)
  (send-message (string-concatenate (list "ERROR " error-code)) out))

(define (send-ok out)
  (send-message "OK" out))

(define (send-ok-w-code code out)
  (display "OK " out)
  (display code out)
  (newline out))

; --- Login ---
(define (validate-or-create-new-user login pass db)
  (let ((users-hash-table (htr db 'users)))
   (let ((user-hash-table (htr/default users-hash-table login #f)))
    (if user-hash-table
      (let ((stored-pass-hash (htr user-hash-table 'pass-hash)))
       (string=? (crypt pass stored-pass-hash) stored-pass-hash)) 
      (let ((new-user-hash-table (make-hash-table)))
       (hts! new-user-hash-table 'pass-hash (crypt pass))
       (hts! new-user-hash-table 'operations (make-hash-table))
       (hts! new-user-hash-table 'operations-by-id (make-hash-table))
       (hts! users-hash-table login new-user-hash-table)
       #t)))))

; --- Operations ---
(define (find-and-add-orders orders-list new-order)
  (let ((orders (hash-table->alist orders-list))
        (value (cadr new-order))
        (quantity (car new-order)))
    (let ((equal-orders
            (filter
              (lambda (v)
                (= (caddr v) value))
              orders)))
      (if (null? equal-orders)
        #f
        (let ((old-order-num (caar equal-orders)))
         (let ((old-order (htr orders-list old-order-num)))
          (set-car! old-order (+ (car old-order) quantity))
          old-order-num))))))

(define (order-constructor user-operations user-operations-by-id db order-type)
  (lambda (active quantity value out)
    (let ((actives-list (htr db 'actives-list)))
     (if (or (eq? order-type 'sell)
             (find (lambda (x) (string=? active (car x))) actives-list))
       (let ((order (list (string->number quantity) (string->number value)))
             (order-num (+ 1 (htr db 'orders-quantity)))
             (user-orders (ht-navigate/create user-operations (list active order-type)))
             (orders (ht-navigate/create db (list 'orders active order-type))))
         (let ((equal-finded (find-and-add-orders user-orders order)))
          (if (not equal-finded)
            (begin
              (hts! orders order-num order)
              (hts! user-orders order-num order)
              (hts! db 'orders-quantity order-num)
              (hts! user-operations-by-id order-num order)
              (send-ok-w-code order-num out))
            (begin
              (hts! user-operations-by-id equal-finded order)
              (send-ok-w-code equal-finded out)))))
       (send-error "005" out)))))

(define (cancel-order-constructor user-operations-by-id db)
  (lambda (order-num)
    (let ((order-num (string->number order-num)))
     (if (hash-table-exists? user-operations-by-id order-num)
       (let ((order (htr user-operations-by-id order-num)))
        (set-car! order '())
        (set-cdr! order '())
        (send-ok))
       (send-error order-dont-exists)))))

(define (show-available-actives db out)
  (let show-first ((available-actives (htr db 'actives-list)))
   (if (null? available-actives)
     (send-ok out)
     (begin
       (display (caar available-actives) out) 
       (newline out)
       (show-first (cdr available-actives))))))

(define (actives user-operations)
  (lambda (active out)
    (let show-list ((actions (hash-table->alist (htr user-operations active))))
     (if (null? actions)
       (send-ok out)
       (begin
         (let ((action (car actions)))
          (cond ((eq? (car action) 'sell)
                 (display "vendas" out))
                ((eq? (car action) 'buy)
                 (display "compras" out)))
          (newline out)
          (display (hash-table->alist (cdr action)) out))
         (newline out)
         (show-list (cdr actions)))))))

(define (list-all-actions active db out)
  (let ((active-table (ht-navigate/create db (list 'orders active))))
   (hash-table-map active-table
    (lambda (action operations)
      (let 
        ((top20-sorted-orders 
           (take
             (sort (hash-table->alist operations)
                   (lambda (v0 v1)
                     (cond ((eq? action 'sell)
                            (< (caddr v0) (caddr v1)))
                           ((eq? action 'buy)
                            (> (caddr v0) (caddr v1))))))
             20)))
       (map 
         (lambda (line)
           (let ((key (car line)) (quantity/value (cdr line)))
            (display key out) (display " " out)
            (cond ((eq? action 'sell)
                   (display "V " out))
                  ((eq? action 'buy)
                   (display "C " out)))
            (display (car quantity/value) out) (display " " out)
            (display (cadr quantity/value) out) (display " " out)
            (newline out)))
         top20-sorted-orders))))))


; --- Sessions ---
(define (construct-session login db out)
  (let ((user-operations (ht-navigate/create db (list 'users login 'operations)))
        (user-operations-by-id (ht-navigate/create db (list 'users login 'operations-by-id))))
    (let ((sell (order-constructor user-operations user-operations-by-id db 'sell))
          (buy (order-constructor user-operations user-operations-by-id db 'buy))
          (actives (actives user-operations))
          (cancel (cancel-order-constructor user-operations-by-id db)))
      (lambda (operation session out)
        (let ((op-type (car operation))
              (args (if (null? (cdr operation))
                      (list out)
                      (append (cdr operation) (list out)))))
          (cond ((string-ci=? op-type "vende")
                 (apply sell args))
                ((string-ci=? op-type "compra")
                 (apply buy args))
                ((string-ci=? op-type "cancela")
                 (apply cancel args))
                ((string-ci=? op-type "catalogo")
                 (show-available-actives db out))
                ((string-ci=? op-type "ativas")
                 (apply actives args))
                ((string-ci=? op-type "lista")
                 (list-all-actions (car args) db out))
                ((string-ci=? op-type "fui")
                 (begin
                   (set-car! session '())
                   (send-ok out)))))))))

(define (get-session login pass db out)
  (if (validate-or-create-new-user login pass db)
    (begin
      (send-ok out)
      (construct-session login db out))
    (send-error invalid-user)))

(define (load-and-get-initial-actions db) 
  (let ((admin-session (get-session "admin" "admin" db (current-output-port))))
   (let ((lines (load-lines "/home/mateus/dev/projeto-pp/ativos")))
    (let add-and-get-action ((quantity (string->number (caar lines)))
                             (actions (cdr lines)))
      (if (or (= quantity 0) (= (length actions) 0))
        (list)
        (let ((action (car actions))
              (rem-actions (add-and-get-action (- quantity 1) (cdr actions))))
          (admin-session (cons "vende" action) '() (current-output-port))
          ; Return a list of lists with the actives to be defined as alist 
          (cons (list (car action)) rem-actions)))))))

(define (initialize-db db)
  (hts! db 'users (make-hash-table))
  (hts! db 'orders (make-hash-table))
  (hts! db 'orders-quantity 0)
  (hts! db 'actives-list '()) ; Set null for load firsts actives
  (hts! db 'actives-list (load-and-get-initial-actions db)))

(define (get-next-command in)
  (string-split (read-line in)))

(define (run-command command db session out)
  (if (null? (car session)) 
    (if (string-ci=? (car command) "login")
      (let ((user (cadr command))
            (pass (caddr command)))
        (set-car! session (get-session user pass db out)))
      (send-error invalid-user out))
    (let ((actual-session (car session)))
     (actual-session command session out)))
  (flush-output out))

(define (run-session session db socket)
  (let-values (((in out) (tcp-accept socket)))
    (let ((command (get-next-command in)))
     (if (string-ci=? (car command) "fui")
       (begin
         (save-db db)
         (send-message "ok" out))
       (begin
         (thread-start!
           (make-thread
             (lambda ()
               (run-command command db session out)
               (close-output-port out))))))))
  (run-session session db socket))

(define (server)
  (let ((db (restore-db)))
   (display "Inicializando")
   (newline)
   (if (= (hash-table-size db) 0)
     (initialize-db db))
   (display "Pronto")
   (newline)
   (let ((session (list '())) (socket (tcp-listen port)))
    (run-session session db socket))))

(server)
