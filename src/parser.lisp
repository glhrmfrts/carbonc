(defpackage carbon.parser
  (:use :cl)
  (:export #:parse-unit))

(in-package :carbon.parser)

(defvar *whitespace* '(#\space #\newline #\tab))

(define-condition smug-condition (simple-condition)
  ((input :reader smug-condition-input
          :initarg :input))
  (:report (lambda (condition stream)
             (apply #'format stream 
                    (simple-condition-format-control condition) 
                    (simple-condition-format-arguments condition))
             (terpri stream)
             (write-string "Input:" stream)
             (print (smug-condition-input condition) stream))))

(define-condition smug-error (error smug-condition) ())

(defgeneric input-empty-p (input)
  (:method ((input string)) (zerop (length input))))

(defgeneric input-first (input)
  (:method ((input string)) (aref input 0)))

(defgeneric input-rest (input)
  (:method ((input string))
    (multiple-value-bind (string displacement) 
        (array-displacement input)
      (make-array (1- (length input))
                  :displaced-to (or string input)
                  :displaced-index-offset (1+ displacement)
                  :element-type (array-element-type input)))))

(defun .error (datum &rest arguments)
  (lambda (input)
    (apply #'error 'smug-error
           :input input 
           (if (or (stringp datum)
                   (functionp datum))
               (list :format-control datum
                     :format-arguments arguments)
               arguments))))

(defun .identity (value)
    (lambda (input)
        (list (cons value input))))

(defun .fail ()
    (lambda (input)
        (declare (ignore input)) nil))

(defun .item ()
  (lambda (input)
    (unless (input-empty-p input)
      (list (cons (input-first input)
		              (input-rest input))))))

(defun .bind (parser function)
  (lambda (input)
    (loop :for (value . input) :in (run parser input)
          :append (run (funcall function value) input))))

(defmacro .let* (bindings &body body)
  (if bindings
      (let ((symbol (first (first bindings))))
        `(.bind ,@(cdr (first bindings))
               (lambda (,symbol)
                 ,@(when (or (string-equal (symbol-name symbol) "_")
                             (null (symbol-package symbol)))
                         `((declare (ignorable ,symbol))))
                 (.let* ,(cdr bindings)
                   ,@body))))
      `(progn ,@body)))

(defmacro .progn (&rest parsers)
    (if (rest parsers)
        (let ((name (gensym)))
          `(.let* ((,name ,(first parsers)))
             (.progn ,@(rest parsers))))
        (first parsers)))

(defmacro .prog1 (parser &rest parsers)
  (let ((name (gensym))
        (ignore (gensym)))
    `(.let* ((,name ,parser)
             (,ignore (.progn ,@parsers)))
       (.identity ,name))))

(defmacro .prog2 (parser1 parser2 &rest parsers)
  (let ((name (gensym))
        (ignore (gensym)))
    `(.let* ((,ignore ,parser1)
             (,name ,parser2)
             (,ignore (.progn ,@parsers)))
       (.identity ,name))))

(defun .satisfies (predicate &rest args)
  (.bind (.item) 
	(lambda (x) 
	  (if (apply predicate x args)
	      (.identity x)
	      (.fail)))))

(defun .is-not (predicate &rest args)
  (.satisfies (lambda (i)
                 (cl:not (apply predicate i args)))))

(defun .is (predicate &rest args)
  (apply #'.satisfies predicate args))

(defun .char= (x)
  (.is #'cl:char= x))

(defun .digit-char-p ()
  (.is #'cl:digit-char-p))

(defun .alpha-char-p ()
  (.is #'alpha-char-p))

(defun .lower-case-p ()
  (.is #'cl:lower-case-p))

(defun .upper-case-p ()
  (.is #'cl:upper-case-p))

(defun .plus (first-parser second-parser)
  (lambda (input)
    (append (funcall first-parser input) (funcall second-parser input))))

(defun .letter () (.plus (.lower-case-p) (.upper-case-p)))

(defun .whitespace (&optional result-type)
  (.first (.map result-type (.is 'member *whitespace*))))

(defun .string= (string)
  (if (string= string "")
      (.identity nil)
      (.let* 
          ((_ (.is 'char= (aref string 0)))
           (_ (.string= (subseq string 1))))
        (.identity string))))

(defun .first (parser)
  (lambda (input)
    (let ((results (run parser input)))
       (when results (list (cl:first results))))))

(defun .zero-or-more (parser)
  (.plus (.let* ((x parser)
                 (xs (.zero-or-more parser)))
           (.identity (cons x xs)))
         (.identity ())))

(defun .one-or-more (parser)
  (.let* ((x parser)
	  (y (.zero-or-more parser)))
    (.identity (cons x y))))

(defun .mapcar (parser)
    (.plus (.let* ((x parser)
                   (xs (.mapcar parser)))
             (.identity (cons x xs)))
           (.identity ())))

(defun .mapc (parser)
    (.plus (.let* ((_ parser)
                   (_ (.mapc parser)))
             (.identity parser))
           (.identity parser)))

(defun .make-list (size &key (initial-element (.item)))
  (if (zerop size) 
      (.identity nil)
      (.let* ((first initial-element)
              (rest (.make-list (1- size) 
                                :initial-element initial-element)))
        (.identity (list* first rest)))))

(defun .make-sequence (type length &key (initial-element (.item)))
  (.let* ((list (.make-list length :initial-element initial-element)))
    (.identity (coerce list type))))

(defun .concatenate (output-type-spec &rest parsers)
  (if (not parsers)
      (.fail)
      (.let* ((first (first parsers))
              (rest (if (rest parsers)
                        (apply 
                         #'.concatenate output-type-spec (rest parsers))
                        (.identity nil))))
        (.identity (cl:concatenate output-type-spec first rest)))))

(defun .map (result-type parser
             &key 
               (at-least 1))
  "=> a ~result-type~ of /parser/ results."
  (.let* ((list-1 (.make-list at-least :initial-element parser))
          (list-2 (funcall (if result-type #'.mapcar #'.mapc) parser)))
    (.identity (when result-type (concatenate result-type list-1 list-2)))))

(defun .or (parser &rest parsers)
  (lambda (input)
    (or (funcall parser input)
        (when parsers 
          (funcall (apply #'.or parsers) input)))))

(defun .not (parser)
  (lambda (input)
    (let ((result (funcall parser input)))
      (if result
	  nil
	  (list (cons t input))))))

(defun .and (p1 &rest ps)
  (.let* ((result p1))
    (if ps
	  (apply #'.and ps)
	  (.identity result))))

(defun .no-more-input ()
 (.not (.item)))

(defun .optional (parser)
  (.or parser (.identity nil)))

(defun .quoted-string (&key (quote #\")
                         (escape #\\))
  (.let* ((_ (.char= quote))
          (string 
           (.map 'string
                 (.plus (.let* ((_ (.char= escape)))
                          (.item))
                        (.is-not 'char= quote))))
          (_ (.char= quote)))
    (.identity (list :string string))))

(defun .float-lit-rest (ch)
  (let ((maybe-dot-parser (if (char= ch (quote #\.)) (.digit-char-p) (.char=(quote #\.)))))
    (.bind (.or (.first (.one-or-more (.or maybe-dot-parser (.digit-char-p)))) (.identity ""))
      (lambda (result)
        ;(format t "~A~%" result)
        (let ((str (remove (quote #\_) (concatenate 'string (list ch) result))))
          (if (some (lambda (ch) (char= ch (quote #\.))) str)
            (.identity (list :float-lit (parse-float:parse-float str)))
            (.identity (list :integer-lit (parse-integer str)))))))))

(defun .float-lit ()
  (.bind (.or (.digit-char-p) (.char=(quote #\.)))
    (lambda (ch)
      (.float-lit-rest ch))))

(defun .integer-lit-rest (ch)
  (.bind (.or (.first (.one-or-more (.or (.char=(quote #\_)) (.digit-char-p)))) (.identity ""))
    (lambda (result)
      ;(format t "~A~%" result)
      (let ((str (remove (quote #\_) (concatenate 'string (list ch) result))))
        (.identity (list :integer-lit (parse-integer str)))))))

(defun .integer-lit ()
  (.bind (.digit-char-p)
    (lambda (ch)
      ;(format t "~A~%" ch)
      (.integer-lit-rest ch))))

(defun .number-lit () (.or (.float-lit) (.integer-lit)))

(defun .if (test-parser then-parser 
            &optional (else-parser (.fail)))
  (let ((no (gensym)))
    (.let* ((no? (.or test-parser (.identity no))))
      (if (not (eq no? no))
          then-parser
          else-parser))))

(defun .when (test-parser then-parser)
   "we define .when in terms of .IF, but it's really just .AND again"
  (.if test-parser then-parser))

(defun .unless (test-parser then-parser)
   "defined in term of .when, even though it's just (.AND (.NOT ...))"
  (.when (.not test-parser) then-parser))

(defun .read-line-newline ()
  (.let* ((line (.optional (.map 'list (.is-not #'char= #\Newline))))
          (newline (.is #'char= #\Newline)))
  (.identity (concatenate 'string line (string newline)))))

(defun .last-line ()
 (.prog1 (.map 'string (.is-not #'char= #\Newline))
         (.not (.item))))

(defun .read-line (&optional 
                     (eof-error-p t)
                     eof-value)
  (.let* ((text (.optional 
                 (.first (.map 'list (.is-not #'char= #\Newline)))))
          (newline (.or (.char= #\Newline)
                        (.and (.not (.item)) 
                              (.identity '())))))
    (if (or text newline)
        (.identity (concatenate 'string text (when newline (string newline))))
        (if eof-error-p 
            (.fail)
            (.identity eof-value)))))

(defun .read (&optional (parser (.sexp)))
  (.prog2
    (.optional (.whitespace))
    parser
    (.optional (.whitespace))))

(defun .token ()
  (.first (.map 'string (.constituent))))

(defun .symbol ()
  (.bind (.first (.token))
    (lambda (result)
      (.identity (list :symbol result)))))

(defun .atom ()
  (.bind
    (.or (.number-lit)
         (.quoted-string)
         (.symbol))
    (lambda (result)
      (.identity result))))

(defvar *non-constituents* 
  (list* #\( #\) *whitespace*))

(defun .constituent (&optional (non-constituents
                                *non-constituents*))
    (.or (.and (.char= #\\) (.item))
         (.is-not 'member non-constituents)))

(defun .list ()
  (.prog2
    (.char= #\( )
    (.read (.first (.zero-or-more (.sexp))))
    (.char= #\) )))

(defun .sexp ()
  (.read (.or (.list) (.atom))))

(defun .sexp-sequence ()
  (.first (.one-or-more (.sexp))))
  
(defun run (parser input)
  (funcall parser input))

(defun parse-unit (input)
  (car (run (.sexp-sequence) input)))