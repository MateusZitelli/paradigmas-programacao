(use crypt) ; For save passwords
(use srfi-1)
(use srfi-69) ; Implements hash-tables
(use tcp srfi-18)

(define port 9010)

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
    (if (or (null? l) (null? h))
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


(define (order-id order) (car order))
(define (order-quantity order) (cadr order))
(define (order-value order) (caddr order))
(define (order-moment order) (cadddr order))

(define (set-order-quantity! order q) (set-car! (cdr order) q))

(define (delete v l)
  (cond
    ((null? l) '())
    ((eq? v (car l)) (delete v (cdr l))) ; Delete if is exactly the same
    (else (cons (car l) (delete v (cdr l))))))

(define (show-db db)
  (display (neasted-hash-table->neasted-alist db))
  (newline))

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

(define (execute-orders active order0 order1 order0-type order1-type db out)
  (let ((active-orders-hash-table
            (ht-navigate/create
              db (list 'active-orders active)))
        (quantity-difference
          (- (order-quantity order0) (order-quantity order1))))
    (cond 
      ((positive? quantity-difference) 
       (begin
         (set-order-quantity! order0 quantity-difference)
         (set-car! (cddr order1) (order-value order0))
         (hts! 
           (ht-navigate/create 
             db (list 'executed-orders active order0-type))
           (car order0) (cdr order0))
         (hash-table-delete! (htr active-orders-hash-table order0-type)
                             (car order0))
         (search-and-execute-orders active order0-type order0 db out)))
      ((negative? quantity-difference)
       (begin
         (set-order-quantity! order1 (- quantity-difference))
         (set-car! (cddr order0) (order-value order1))
         (hts! 
           (ht-navigate/create 
             db (list 'executed-orders active order1-type))
           (car order1) (cdr order1))
         (hash-table-delete! (htr active-orders-hash-table order1-type)
                             (car order1))
         (search-and-execute-orders active order0-type order0 db out)))
      (else 
        (begin
          (let ((mean-value (/ (+ (order-value order0)
                                  (order-value order1)) 2))) 
            (set-car! (cddr order0) mean-value)
            (set-car! (cddr order1) mean-value))

          (hts! 
            (ht-navigate/create 
              db (list 'executed-orders active order0-type))
            (car order0) (cdr order0))
          (hash-table-delete! active-orders-hash-table
                              (car order0))
          (hts!  
            (ht-navigate/create 
              db (list 'executed-orders active order1-type))
            (car order1) (cdr order1))
          (hash-table-delete! active-orders-hash-table
                              (car order1)))))))


(define (search-and-execute-orders active order-type order db out)
  (let ((other-order-type (if (eq? order-type 'sell) 'buy 'sell))
        (order-comparator
          (if (eq? order-type 'sell) 
            (lambda (ord0 ord1) (<= (order-value ord0) (order-value ord1)))
            (lambda (ord0 ord1) (>= (order-value ord0) (order-value ord1))))))
    (let ((active-orders-list
            (hash-table->alist
              (ht-navigate/create
                db (list 'active-orders active other-order-type))))
          (order-fold-function
            (lambda (ord0 ord1)
              (if (order-comparator ord0 ord1) ord0 ord1)))
          (order-filter-function
            (lambda (other-order)
              (order-comparator order other-order))))
      (if (>= (length active-orders-list) 1)
        (let ((filtered-orders
                (filter order-filter-function active-orders-list)))
          (if (not (null? filtered-orders))
            (let ((best-order
                    (fold order-fold-function
                          (car filtered-orders)
                          (cdr filtered-orders))))
              (execute-orders
                active order best-order order-type other-order-type db out))))))))


; --- Operations ---
(define (get-order-constructor user-operations user-operations-by-id db type)
  (lambda (active quantity value out)
    (let 
     ((actives-list (hash-table->alist (htr db 'active-orders)))
      (find-and-add-orders
        (lambda (user-orders new-order)
          (let ((orders (hash-table->alist user-orders))
                (value (car new-order))
                (quantity (order-quantity new-order)))
            (let ((equal-orders
                    (filter
                      (lambda (v) (= (caddr v) value))
                      orders)))
              (if (null? equal-orders)
                #f
                (let ((old-order-num (caar equal-orders)))
                 (let ((old-order (htr user-orders old-order-num)))
                  (set-car! old-order (+ (car old-order) quantity))
                  old-order-num))))))))
     (if (or (eq? type 'sell)
             (find (lambda (x) (string=? active (car x))) actives-list))
       (let ((order (list
                      (string->number quantity)
                      (string->number value)
                      (time->seconds (current-time))))
             (order-num (+ 1 (htr db 'last-order-id)))
             (user-orders
               (ht-navigate/create user-operations (list active type)))
             (orders 
               (ht-navigate/create db (list 'active-orders active type))))
         (let ((equal-finded (find-and-add-orders user-orders order)))
          (if equal-finded
            (begin
              (hts! user-operations-by-id equal-finded order)
              (search-and-execute-orders
                active type (cons equal-finded order) db out)
              (send-ok-w-code equal-finded out))
            (begin
              (hts! orders order-num order)
              (hts! user-orders order-num order)
              (hts! db 'last-order-id order-num)
              (hts! user-operations-by-id order-num order)
              (search-and-execute-orders
                active type (cons order-num order) db out)
              (send-ok-w-code order-num out)))))
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
  (let show-first
   ((available-actives (hash-table->alist (htr db 'active-orders))))
   (if (null? available-actives)
     (send-ok out)
     (begin
       (display (caar available-actives) out) 
       (newline out)
       (show-first (cdr available-actives))))))

(define (show-user-orders-in orders-table user-operations active out)
   (let show-list ((actions (hash-table->alist (htr user-operations active))))
    (if (null? actions)
      (send-ok out)
      (begin
        (let ((action (car actions)))
         (let
          ((order-type-string (cond ((eq? (car action) 'sell) "V")
                                    ((eq? (car action) 'buy) "C")))
           (display-to-out-w-space (lambda (s)
                                     (display s out) (display " " out))))
          (let display-orders ((orders (hash-table->alist (cdr action))))
           (if (null? orders)
             '()
             (let ((order (car orders)))
              (if (hash-table-exists?
                    ; Get orders of a certain type of an active
                    (ht-navigate/create 
                      orders-table
                      (list active (car action)))
                    ; Check if there is an operation with the same id
                    (order-id order))
                (begin
                  (map display-to-out-w-space
                       (list (order-id order)
                             active
                             (order-quantity order)
                             order-type-string
                             (order-value order)
                             (order-moment order)))
                  (newline out)))
              (display-orders (cdr orders)))))))
        (show-list (cdr actions))))))

(define (activated-orders user-operations db)
  (lambda (active out)
    (show-user-orders-in (htr db 'active-orders) user-operations active out)))

(define (executed-orders user-operations db)
  (lambda (active out)
    (show-user-orders-in (htr db 'executed-orders) user-operations active out)))

(define (list-all-actions active db out)
  (let ((active-table (ht-navigate/create db (list 'active-orders active))))
   (hash-table-map 
     active-table
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

(define (cotation active db out)
  (let 
   ((executed-orders (ht-navigate/create db (list 'executed-orders active))))
   (let 
    ((executed-buys-list 
       (if (hash-table-exists? executed-orders 'buy) 
         (hash-table->alist (htr executed-orders 'buy))
         (list)))
     (executed-sells-list 
       (if (hash-table-exists? executed-orders 'sell) 
         (hash-table->alist (htr executed-orders 'sell))
         (list)))
     (get-most-recent 
       (lambda (o0 o1)
         (cond
           ((and (null? o0) (null? o1)) (list))
           ((null? o0) o1)
           ((null? o1) o0)
           ((> (order-moment o0) (order-moment o1)) o0)
           (else o1)))))
    (let
     ((last-buy-order 
        (if (not (null? executed-buys-list))
          (fold get-most-recent 
                (car executed-buys-list)
                (cdr executed-buys-list))
          (list)))
      (last-sell-order 
        (if (not (null? executed-sells-list))
          (fold get-most-recent 
                (car executed-sells-list)
                (cdr executed-sells-list))
          (list))))
     (let ((most-recent (get-most-recent last-buy-order last-sell-order))
           (display-to-out (lambda (s) (display s out))))
       (if (null? most-recent)
         (begin
           (map display-to-out (list active " " -1 " " -1 " " 0))
           (newline out))
         (begin
           (map display-to-out
                (list
                  active " "
                  (order-value most-recent) " "
                  (order-quantity most-recent) " "
                  (order-moment most-recent)))
           (newline out))))))))

; --- Sessions ---
(define (construct-session login db out)
  (let ((user-operations 
          (ht-navigate/create db (list 'users login 'operations)))
        (user-operations-by-id
          (ht-navigate/create db (list 'users login 'operations-by-id))))
    (let 
     ((sell
        (get-order-constructor user-operations user-operations-by-id db 'sell))
      (buy
        (get-order-constructor user-operations user-operations-by-id db 'buy))
      (activated-orders (activated-orders user-operations db))
      (executed-orders (executed-orders user-operations db))
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
                (apply activated-orders args))
               ((string-ci=? op-type "executadas")
                (apply executed-orders args))
               ((string-ci=? op-type "lista")
                (list-all-actions (car args) db out))
               ((string-ci=? op-type "cotacao")
                (cotation (car args) db out))
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
  (let ((admin-session (get-session "admin" "admin" db (current-output-port)))
        (lines (load-lines "/home/mateus/dev/projeto-pp/ativos")))
    (let add-and-get-action
     ((quantity (string->number (caar lines)))
      (actions (cdr lines)))
     (if (or (= quantity 0) (= (length actions) 0))
       (list)
       (let ((action (car actions))
             (rem-actions (add-and-get-action (- quantity 1) (cdr actions))))
         (admin-session (cons "vende" action) '() (current-output-port))
         ; Return a list of lists with the actives to be defined as alist 
         (cons (list (car action)) rem-actions))))))

(define (initialize-db db)
  (hts! db 'users (make-hash-table))
  (hts! db 'active-orders (make-hash-table))
  (hts! db 'last-order-id 0)
  (hts! db 'executed-orders (make-hash-table)) 
  (load-and-get-initial-actions db))

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
