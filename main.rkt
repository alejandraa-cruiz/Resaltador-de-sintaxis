#lang racket

(require 2htdp/batch-io)
(require racket/string)

(define keywords 
  "auto|const|double|int|float|short|struct|unsigned|break|continue|else|for|long|
   signed|switch|void|case|default|enum|goto|register|sizeof|typedef|volatile|char|do|extern|if|return|static|
   union|while|asm|dynamic_cast|namespace|reinterpret_cast|try|bool|explicit|new|static_cast|typeid|catch|false|
   operator|template|typename|class|friend|private|this|using|const_cast|inline|public|throw|virtual|delete|
   mutable|protected|true|wchar_t")

(define operators "+|-|*|/|%|^|&|~|!|=|<|>|?|:|;|,|.|++|--|&&|==|!=|<=|>=|+=|-=|*=|/=|%=|^=|&=|<<=|>>=|=>|??")

(define delimiters "(|)|[|]|{|}")

(define input-file "C:\\Users\\Salvador\\testcode.cs")

 ;;parse the file
(define (file->list-of-chars filename)
    (flatten
     (map string->list
          (read-1strings filename))))

(define (code->lines code)
    (regexp-split #rx"\n" code))

(define code (apply string (file->list-of-chars input-file)))


(define add-line
    (lambda (lst line)
        (list line)))

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