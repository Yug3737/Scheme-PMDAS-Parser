#lang scheme
(require irregex)

;; TOKENIZER

(define expr-str (read-line))

(display "You Entered: ")
(display expr-str)

(newline)
(if (string? expr-str)
    (void)
    (error "InputError" "Input is not valid."))

(define-struct token (tag value))

(define patterns
  (list (list "\\(" "(")
        (list "\\)" ")")
        (list "\\+" "+")
        (list "\\-" "-")
        (list "\\*" "*")
        (list "\\/" "/")
        (list " " "whitespace")
        (list "\\d+" "number")))


(define match-all-patterns (lambda (char-str patterns-lst)
                             (define empty (null? patterns-lst))
                             (if empty
                                 (error "InputError" "Input is invalid. Cannot find token.")
                                 (void))
                             (define match (irregex-match (caar patterns-lst) char-str))
                             (list? match)
                             (if match
                                 (let ((token (make-token (cadar patterns-lst)
                                                           char-str)))
                                   token)
                                 (match-all-patterns char-str (cdr patterns-lst)))))


(define split-string (lambda (str)
  (define (helper chars)
    (if (null? chars)
        '()
        (cons (string (car chars))
              (helper (cdr chars)))))
  (helper (string->list str))))


(define tokenize
  (lambda (char-lst tokens-lst)
    (define empty (null? char-lst))
    (if empty
        tokens-lst
        (let ((token (match-all-patterns (car char-lst) patterns)))
          (if token
              (tokenize (cdr char-lst) (append tokens-lst (list token)))
              (tokenize (cdr char-lst) tokens-lst))))))

(define expr-lst (split-string expr-str))
(define tokens (tokenize expr-lst '()))


;; PARSER

(define-struct node (tag value left right))

(define parse-simple-expression (lambda (tokens-lst)
                                  (define token0 (car tokens-lst))
                                  (define token0-tag (token-tag token0))
                                  (define token0-value (token-value token0))
                                  (cond
                                    ((equal? token0-tag "number")
                                     (values (make-node "number" token0-value null null)
                                             (cdr tokens-lst)))
                                    ((equal? token0-tag "(")
                                     (begin
                                       (define-values (node remaining-tokens) (parse-term (cdr tokens-lst)))
                                       (if (not (equal? (token-tag (car remaining-tokens)) ")"))
                                           (error (string-append "Error: expected ')' but got "
                                                                 (token-tag (car remaining-tokens))
                                                                 " token")
                                                  (token-value (car remaining-tokens)))
                                           (void)
                                           )
                                       (values node (cdr remaining-tokens))))
                                    ((equal? token0-tag "-")
                                     (begin
                                       (define-values (node remaining-tokens) (parse-simple-expression (cdr tokens-lst)))
                                       (values (make-node "negate" "-" node null)
                                             remaining-tokens)))
                                    (else (error "Invalid token" token0-value)))))


(define parse-term
  (lambda (tokens-lst)
    (define (loop current-node remaining-tokens)
      (if (null? remaining-tokens)
          (values current-node remaining-tokens)
          (let ((next-token (car remaining-tokens)))
            (if (member (token-tag next-token) '("*" "/"))
                (let ((token0-tag (token-tag next-token)))
                  (define-values (right-node remaining-tokens2)
                    (parse-simple-expression (cdr remaining-tokens)))
                  (define new-node (make-node token0-tag token0-tag current-node right-node))
                  (loop new-node remaining-tokens2))
                (values current-node remaining-tokens)))))
    (define-values (node remaining-tokens) (parse-simple-expression tokens-lst))
    (loop node remaining-tokens)))



(define parse-arithmetic-expression
  (lambda (tokens-lst)
    (define (loop current-node remaining-tokens)
      (if (null? remaining-tokens)
          (values current-node remaining-tokens)
          (let ((next-token (car remaining-tokens)))
            (if (member (token-tag next-token) '("+" "-"))
                (let ((token0-tag (token-tag next-token)))
                  (define-values (right-node remaining-tokens2)
                    (parse-term (cdr remaining-tokens)))
                  (define new-node (make-node token0-tag token0-tag current-node right-node))
                  (loop new-node remaining-tokens2))
                (values current-node remaining-tokens)))))
    (define-values (node remaining-tokens) (parse-term tokens-lst))
    (loop node remaining-tokens)))

; EVALUATOR

(define evaluate
  (lambda (ast)
    (define ast-tag (node-tag ast))
    (define ast-value (node-value ast))
    (define ast-left (node-left ast))
    (define ast-right (node-right ast))
    (cond
      ((equal? ast-tag "number")
         ast-value)
      ((equal? ast-tag "+")
       (begin
         (define left-value (evaluate ast-left))
         (define right-value (evaluate ast-right))
         (number->string (+ (string->number left-value) (string->number right-value)))))
      ((equal? ast-tag "-")
       (begin
         (define left-value (evaluate ast-left))
         (define right-value (evaluate ast-right))
         (number->string (- (string->number left-value) (string->number right-value)))))
      ((equal? ast-tag "*")
       (begin
         (define left-value (evaluate ast-left))
         (define right-value (evaluate ast-right))
         (number->string (* (string->number left-value) (string->number right-value)))))
      ((equal? ast-tag "/")
       (begin
         (define left-value (evaluate ast-left))
         (define right-value (evaluate ast-right))
         (number->string (/ (string->number left-value) (string->number right-value)))))
      ((equal? ast-tag "negate")
       (begin
         (define left-value (evaluate ast-left))
         (number->string (* (string->number left-value) -1))))
      (else (error "\nUnknown operator in AST."))
      )))


(define print-tokens
  (lambda (tokens-lst)
    (for-each
     (lambda (token)
       (display (string-append "Tag: " (token-tag token) ", Value: " (token-value token) "\n")))
     tokens-lst)))

(define print-node
  (lambda (node)
    (cond
      ((empty? node) (display "Empty Node"))
      (else
       (begin
         (display "(Tag:")
         (display (node-tag node))
         (display " Value:")
         (display (node-value node))
         (display " Left:")
         (display (node-left node))
         (display " Right:")
         (display (node-right node))
         (display ")"))))))

(define print-ast
  (lambda (ast)
    (cond
      ((empty? ast) (display "empty"))
      ((node? ast)
       (begin
         (display "(Tag:")
         (display (node-tag ast))
         (display " Node:")
         (display (node-value ast))
         (display " Left:")
         (print-ast (node-left ast))
         (display " Right:")
         (print-ast (node-right ast))
         (display ")")))
      (else
       (error "Invalid AST structure" ast)))))



(define-values (result-ast remaining-tokens) (parse-arithmetic-expression tokens))
(newline)
(print-ast result-ast)
(newline)
(print-tokens remaining-tokens)
(newline)


(display (evaluate result-ast))
