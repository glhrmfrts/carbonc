(defpackage carbon.main
  (:use :cl :carbon.parser)
  (:export #:compile-file-contents
           #:print-ts))

(in-package :carbon.main)

(defun is-defun (f)
  (and (listp f) (and (equal :symbol (car f)) (string= "defun" (second f)))))

(defun produce-defun-node (content)
  (let ((sym (first content))
        (arglist (first (rest content)))
        (body (rest (rest content))))
    (list :defun nil sym arglist (ast-to-semtree body))))

(defun semantic-node (node)
  (if (or (not (listp node)) (= 0 (length node)))
    node
    (destructuring-bind (k &rest content) node
      (cond
        ((is-defun k)            (produce-defun-node content))
        ((listp (first content)) (list :call nil (semantic-node k) (ast-to-semtree content)))
        (t                       (list k nil (first content)))))))

(defun ast-to-semtree (ast)
  (map 'list #'semantic-node ast))


(defstruct type-info name kind intrkind size alignment signed arg-validators)

(defstruct local-info name tid)

(defstruct lex-scope kind types locals)

(defstruct type-system scopes curscope)

(defparameter *ts*                (make-type-system :scopes (make-array 32) :curscope -1))
(defparameter *builtin-scope-idx* 0)
(defparameter *unresolved-types*  nil)

(defun make-integral-type-info (name sz align signed)
  (make-type-info :name name :kind :integral :size sz :alignment align :signed signed))

(defun make-intrinsic-type-info (name intrkind arg-validators)
  (make-type-info :name name :kind :intrinsic :intrkind intrkind :arg-validators arg-validators))

(defun make-local-of-type (name tid)
  (make-local-info :name name :tid tid))

(defun add-scope (kind)
  (progn
    (setf (aref (type-system-scopes *ts*) (+ (type-system-curscope *ts*) 1)) (make-lex-scope :kind kind
                                                                                             :types (make-hash-table)
                                                                                             :locals (make-hash-table)))
    (setf (type-system-curscope *ts*) (+ (type-system-curscope *ts*) 1))))

(defun current-scope ()
  (aref (type-system-scopes *ts*) (type-system-curscope *ts*)))

(defun register-type (id info)
  (setf (gethash id (lex-scope-types (current-scope))) info))

(defun register-local-of-type (name tid)
  (setf (gethash name (lex-scope-locals (current-scope))) (make-local-of-type name tid)))

(defun is-type-kind (info kind)
  (destructuring-bind (n k &rest rest) info
    (declare (ignore n rest))
    (equal k kind)))

(defun is-integral-type (info)
  (destructuring-bind (n k &rest rest) info
    (declare (ignore n rest))
    (equal k :integral)))

(defun create-builtin-types ()
  (progn
    (add-scope :builtin)
    (register-type "bool" (make-integral-type-info "bool" 1 1 t))
    (register-type "int"  (make-integral-type-info "int" 4 4 t))
    (register-type "uint" (make-integral-type-info "uint" 4 4 nil))

    (register-type "+"            (make-intrinsic-type-info "+" :add (list #'is-integral-type #'is-integral-type)))
    (register-local-of-type "+"   (list *builtin-scope-idx* "+"))

    t))

(defun init-type-system ()
  (create-builtin-types))

(defun print-ts ()
  (format t "~A~%" *ts*))

(defun get-node-kind (node)
  (first node))

(defun get-node-type (node)
  (second node))

(defun every-type (nodes)
  (every (lambda (it) (not (null (get-node-type it)))) nodes))

(defun type-symbol (id)
  (list :symbol '(0 "int") id))

(defun type-intrinsic (fn args)
  (let* ((tfn           (get-node-type fn))
         (validators    (type-info-arg-validators tfn))
         (argtypes      (map 'list #'get-node-type args))
         (argval        (mapcar #'list argtypes validators))
         (args-match    (every (lambda (it) (funcall (second it) (first it))) argval)))
    (cond 
      (args-match       (list :intrinsic (first argtypes) (type-info-intrkind tfn) args))
      (t                (list :call nil fn args)))))

(defun type-call (fn args)
  (let* ((tfn               (type-node fn))
         (targs             (type-tree args))
         (all-resolved      (and (not (null (get-node-type tfn))) (every-type targs))))
    (cond
      ((and all-resolved (is-type-kind (get-node-type tfn) :intrinsic)) (type-intrinsic tfn targs))
      (t                                                                (list :call nil tfn targs)))))

(defun type-node (node)
  (if (or (not (listp node)) (< (length node) 2))
    node    
    (destructuring-bind (k typ &rest content) node
      (declare (ignore typ))
      (format t "~A~%" content)
      (cond
        ((equal :integer-lit k)  (list k (list *builtin-scope-idx* "int") (first content)))
        ((equal :float-lit k)    (list k (list *builtin-scope-idx* "float") (first content)))
        ((equal :symbol k)       (type-symbol (first content)))
        ((equal :call k)         (type-call (first content) (second content)))
        ((listp k)               (list (type-node k) nil (type-tree content)))
        ((listp (first content)) (list (type-node k) nil (type-tree content)))
        (t                       (list (type-node k) nil (type-node (first content))))))))

(defun type-tree (ast)
  (map 'list #'type-node ast))


(defun compile-file-contents (src)
  (let* ((ast (parse-unit src))
         (semtree (ast-to-semtree (car ast))))
    (init-type-system)
    (format t "~A~%" ast)
    (format t "~A~%" semtree)
    (format t "~A~%" (type-tree semtree))
    (type-tree semtree)))