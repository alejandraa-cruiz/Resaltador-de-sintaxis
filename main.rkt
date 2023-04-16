#lang racket

(require 2htdp/batch-io)
(require racket/string)

(define input-file "C:\\Users\\Salvador\\testcode.cs")

;;parses the file
(define (file->list-of-chars filename)
    (flatten
     (map string->list
          (read-1strings filename))))

;;separates the file my lines
(define (code->lines code)
    (regexp-split #rx"\n" code))

;;contacat all the clases
(define (anyclass classes)
    (cond
    ((empty? (cdr classes)) (car(car classes)))
    (else (string-append  (car(car classes)) "|" (anyclass (cdr classes))))
      )
    )
  
;;list of list with all the c# syntaxis clases
(define classes
(list
  (list "\\/\\/.*" "comment")
  (list "\".*\"" "string")
  (list "\\s+" "space" )
  (list "[+-]?(?=\\d*[.eE])(?=\\.?\\d)\\d*\\.?\\d*(?:[eE][+-]?\\d+)?"  "float" )
  (list "[\\-|\\+]?\\d+"  "int" )
  (list "[\\(|\\)|\\#|\\;|\\<|\\>|\\,\\[|\\]|\\{|\\}\\.|\\!]"  "delimiter" )
  (list "auto|const|double|int|float|short|struct|unsigned|break|continue|else|for|long|
   signed|switch|void|case|default|enum|goto|register|sizeof|typedef|volatile|char|do|extern|if|return|static|
   union|while|asm|dynamic_cast|namespace|reinterpret_cast|try|bool|explicit|new|static_cast|typeid|catch|false|
   operator|template|typename|class|friend|private|this|using|const_cast|inline|public|throw|virtual|delete|
   mutable|protected|true|wchar_t|string|" "reserved" )
  (list "[a-zA-Z][a-zA-Z0-9_]*"  "variable" )
  (list "[\\-|\\+|\\^|\\=|\\/|\\*]"  "opertator" )))

;;executes function classify-line to all elements in the list
(define (clasify-code-by-line code-by-line)
    (map clasify-line code-by-line))

;;executes function classify-element to all elements in the list
(define (clasify-line line-code)
    (map clasify-element (separate-tokens line-code)))

;;match the code with the regex values
(define (separate-tokens code)
    (regexp-match* (pregexp (anyclass classes))  code))

;;executes function clasify-element-aux to all elements in the list
(define (clasify-element element)
   (clasify-element-aux element classes))

;;match the code with the regex values and assigs a class
(define (clasify-element-aux element list-classes)
    (cond 
      ((empty? list-classes) (list element "undefined") )
      ((regexp-match? (pregexp (car(car list-classes))) element) (list element (cadr(car list-classes)) ))
      (else (clasify-element-aux element (cdr list-classes)))))

;;reverses the created list in ordr to get the right order
(define (pack content)
    (cond
    ((empty? content) '())
    (else (pack-aux (cdr (reverse content)) (list(car (reverse content)))))))

;;creates a list with the code and their assign classes
(define (pack-aux content mem)
    (cond
    ((empty? content) mem)
    ( (and (eq? (cadr(car content)) (cadr(car mem))) (eq? (cadr(car content)) "int") (regexp-match? #px"[\\-|\\+]\\d+" (car (car mem)))  )
      (pack-aux (cdr content) (append (list   (list (car (car content))  "int") (list (car(regexp-match #px"[\\-|\\+]" (car (car mem)))) "operator") (list (car(regexp-match #px"\\d+" (car (car mem))))  "int") )  (cdr mem)) ))
    
    ((eq? (cadr(car content)) (cadr(car mem))) (pack-aux (cdr content) (append (list   (list (string-append (car (car content)) " " (car (car mem)))  (cadr(car mem)) ))  (cdr mem)) ) )
    (else (pack-aux (cdr content) (append (list (car content)) mem )))))

;;define code as a constant
(define code (apply string (file->list-of-chars input-file)))

;;(map pack(clasify-code-by-line (code->lines code)))

;;add string span class
(define string-span
  (lambda (token)
    (cond
      [(regexp-match* #rx"^\".*\"$" token) (string-append "<span class=string>" token "</span>")]
      [else token])))

;;add numbers span class
(define number-span
  (lambda (token)
    (cond
      [(regexp-match* #rx"^[0-9x]+$" token) (string-append "<span class=number>" token "</span>")]
      [else token])))

;;add operators a span
(define operator-span
  (lambda (token)
    (cond
      [(member token operators) (string-append "<span class=operator>" token "</span>")]
      [else token])))

;;add operatodelimiters a span
(define delimiter-span
  (lambda (token)
    (cond
      [(member token delimiters) (string-append "<span class=delimiter>" token "</span>")]
      [else token])))

;;add keywords a span
(define keyword-span
  (lambda (token)
    (cond
      [(member token keywords) (string-append "<span class=keyword>" token "</span>")]
      [else token])))

;;add comment a span
(define comment-span
  (lambda (list)
    (define text(string-join list))
    (string-append "<span class=comment>" text "</span>")))

;(format-line '("//" "hola" "" "ale"))
;;format-line
(define format-line
  (lambda (line)
    (define pattern (regexp-quote "//"))
    (define elem (first line))
    (cond
      [(regexp-match? pattern elem)
        (comment-span line)]
      [else
       (for ([token line])
         (cond
           [(keyword-span token)]
           [(string-span token)]
           [(number-span token)]
           [(delimiter-span token)]
           [(operator-span token)]))])))
          

;;ads all the lines in the list of lines to our html
(define(write-html file lines)
    (define output (open-output-file file))
    (for ([each lines])
        (displayln each output))
        (close-output-port output)
)

(define(main input output)
  (
    
  ))