(use crypt) ; For save passwords
(use srfi-1)
(use srfi-69) ; Implements hash-tables

; --- Errors ----
(define invalid-user "001")
(define order-dont-exists "004")

; --- Alias ---
(define hts! hash-table-set!)
(define htr hash-table-ref)
(define htr/default hash-table-ref/default)

; --- Utils ---
(define ht-navigate/create
  (lambda (hash path-list)
    (let enter ((h hash)
                (l path-list))
      (if (null? l)
        h
        (let ((next-key (car l)))
         (if (hash-table-exists? h next-key)
          (enter (htr h next-key) (cdr l))
          (begin
           (hts! h next-key (make-hash-table))
           (enter (htr h next-key) (cdr l)))))))))

; --- File handling functions ---
(define save-data
  (lambda (configs file-name)
    (let ((out (open-output-file file-name)))
     (write configs out)
     (close-output-port out))))

(define load-data
  (lambda (file-name)
    (let ((in (open-input-file file-name)))
     (let ((data (read in)))
      (close-input-port in)
      data))))

; TODO: Optimizate to use tail call
(define neasted-hash-table->neasted-alist
  (lambda (h)
    (let ((alist (hash-table->alist h)))
     (let iterate ((l alist))
      (if (= (length l) 0)
        '()
        (begin
          (if (hash-table? (cdar l))
            (set-cdr! (car l) (neasted-hash-table->neasted-alist (cdar l))))
          (cons (car l) (iterate (cdr l)))))))))

; TODO: Implement with tail call
(define neasted-alist->neasted-hash-table
  (lambda (alist)
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
            h)))))))

(define save-hash-table
  (lambda (hash file-name)
    (save-data (neasted-hash-table->neasted-alist hash) file-name)))

(define load-hash-table
  (lambda (file-name)
    (neasted-alist->neasted-hash-table (load-data file-name))))

(define restore-data
  (lambda (file-name)
    (if (file-exists? file-name)
      (load-hash-table file-name)
      (make-hash-table))))

(define restore-db 
  (lambda ()
    ; not working yet(restore-data "main.dbscm")
    (make-hash-table)))

(define save-db
  (lambda (db) (save-hash-table db "main.dbscm")))

(define split-lines-from-port
  (lambda (port)
    (let read-next-line ()
     (let ((line (read-line port)))
      (if (eof-object? line)
        (list)
        (let ((action (string-split line)))
         (cons action (read-next-line))))))))

; Return a list of the splited lines contened in a file
(define load-lines
  (lambda (actions-file)
    (call-with-input-file actions-file split-lines-from-port)))

; --- Server responses ---
(define send-message
  (lambda (message)
    (display message)
    (newline)))

(define send-error
  (lambda (error-code)
    (send-message (string-concatenate (list "ERROR " error-code)))))

(define send-ok
  (lambda ()
    (send-message "OK")))

(define send-ok-w-code
  (lambda (code)
    (display "OK ")
    (display code)
    (newline)))

; --- Login ---
(define validate-or-create-new-user
  (lambda (login pass db)
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
         #t))))))

; --- Operations ---
(define find-and-add-orders
  (lambda (orders-list new-order)
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
           old-order-num)))))))

(define order-constructor
  (lambda (user-operations user-operations-by-id db order-type)
    (lambda (active quantity value)
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
               (send-ok-w-code order-num))
             (begin
               (hts! user-operations-by-id equal-finded order)
               (send-ok-w-code equal-finded)))))
        (send-error "005"))))))

(define cancel-order-constructor
  (lambda (user-operations-by-id db)
    (lambda (order-num)
      (let ((order-num (string->number order-num)))
       (if (hash-table-exists? user-operations-by-id order-num)
        (let ((order (htr user-operations-by-id order-num)))
         (set-car! order '())
         (set-cdr! order '())
         (send-ok))
        (send-error order-dont-exists))))))

(define show-available-actives
  (lambda (db)
    (let show-first ((available-actives (hash-table-keys (htr db 'actives-list))))
    (if (null? available-actives)
      (send-ok)
      (begin
        (display (car available-actives)) 
        (newline)
        (show-first (cdr available-actives)))))))

(define actives
  (lambda (user-operations)
    (lambda (active)
      (let show-list ((actions (hash-table-values (htr user-operations active))))
       (if (null? actions)
         (send-ok)
         (begin
           (display (cons active (cdar (hash-table->alist (car actions)))))
           (newline)
           (show-list (cdr actions))))))))
; --- Sessions ---
(define construct-session
  (lambda (login db)
    (let ((user-operations (ht-navigate/create db (list 'users login 'operations)))
          (user-operations-by-id (ht-navigate/create db (list 'users login 'operations-by-id))))
     (let ((sell (order-constructor user-operations user-operations-by-id db 'sell))
           (buy (order-constructor user-operations user-operations-by-id db 'buy))
           (actives (actives user-operations))
           (cancel (cancel-order-constructor user-operations-by-id db)))
      (lambda (operation session)
        (let ((op-type (car operation))
              (args (cdr operation)))
          (cond ((string-ci=? op-type "vende")
                 (apply sell args))
                ((string-ci=? op-type "compra")
                 (apply buy args))
                ((string-ci=? op-type "cancela")
                 (apply cancel args))
                ((string-ci=? op-type "catalogo")
                 (show-available-actives db))
                ((string-ci=? op-type "fui")
                 (begin
                   (set-car! session '())
                   (send-ok)))
                ((string-ci=? op-type "ativas")
                 (apply actives args)))))))))

(define get-session
  (lambda (login pass db)
    (if (validate-or-create-new-user login pass db)
      (begin
        (send-ok)
        (construct-session login db))
      (send-error invalid-user))))

(define load-and-get-initial-actions
  (lambda (db) 
    (let ((admin-session (get-session "admin" "admin" db)))
     (let ((lines (load-lines "/home/mateus/dev/projeto-pp/ativos")))
      (let add-and-get-action ((quantity (string->number (caar lines)))
                       (actions (cdr lines)))
        (if (or (= quantity 0) (= (length actions) 0))
          (list)
          (let ((action (car actions))
                (rem-actions (add-and-get-action (- quantity 1) (cdr actions))))
           (admin-session (cons "vende" action) '())
           ; Return a list of lists with the actives to be defined as alist 
           (cons (list (car action)) rem-actions))))))))

(define initialize-db
  (lambda (db)
    (hts! db 'users (make-hash-table))
    (hts! db 'orders (make-hash-table))
    (hts! db 'orders-quantity 0)
    (hts! db 'actives-list '()) ; Set null for load firsts actives
    (hts! db 'actives-list (load-and-get-initial-actions db))))

(define get-next-command
  (lambda ()
    (string-split (read-line (current-input-port)))))

(define run-command
  (lambda (command db session)
    (if (null? (car session)) 
      (if (string-ci=? (car command) "login")
        (let ((user (cadr command))
              (pass (caddr command)))
          (set-car! session (get-session user pass db)))
        (send-error invalid-user))
      (let ((actual-session (car session)))
       (actual-session command session)))))

(define server
  (lambda ()
    (let ((db (restore-db)))
     (display "Inicializando")
     (newline)
     (if (= (hash-table-size db) 0)
       (initialize-db db))
     (display "Pronto")
     (newline)
     (let ((session (list '())))
      (let process-command ((command (get-next-command)))
       (if (string-ci=? (car command) "fui") 
         (begin
           (save-db db)
           (send-message "ok"))
         (begin
           (run-command command db session)
           (process-command (get-next-command)))))))))

(server)