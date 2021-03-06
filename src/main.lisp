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


;;; Types

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
                                                                                             :types (make-hash-table :test #'equal)
                                                                                             :locals (make-hash-table :test #'equal)))
    (setf (type-system-curscope *ts*) (+ (type-system-curscope *ts*) 1))))

(defun get-scope (idx)
  (aref (type-system-scopes *ts*) idx))

(defun current-scope ()
  (get-scope (type-system-curscope *ts*)))

(defun register-type (id info)
  (setf (gethash id (lex-scope-types (current-scope))) info))

(defun register-local-of-type (name tid)
  (setf (gethash name (lex-scope-locals (current-scope))) (make-local-of-type name tid)))

(defun is-type-kind (info kind)
  (equal kind (type-info-kind info)))

(defun is-integral-type (info)
  (equal :integral (type-info-kind info)))

(defun create-builtin-types ()
  (progn
    (add-scope :builtin)
    (register-type "bool" (make-integral-type-info "bool" 1 1 t))
    (register-type "int"  (make-integral-type-info "int" 4 4 t))
    (register-type "uint" (make-integral-type-info "uint" 4 4 nil))

    (register-type "+"  (make-intrinsic-type-info "+" :add (list #'is-integral-type #'is-integral-type)))
    (register-type "-"  (make-intrinsic-type-info "-" :sub (list #'is-integral-type #'is-integral-type)))
    (register-type "*"  (make-intrinsic-type-info "*" :mul (list #'is-integral-type #'is-integral-type)))

    (register-local-of-type "+"   (list *builtin-scope-idx* "+"))
    (register-local-of-type "-"   (list *builtin-scope-idx* "-"))
    (register-local-of-type "*"   (list *builtin-scope-idx* "*"))

    t))

(defun init-type-system ()
  (create-builtin-types))

(defun print-ts ()
  (format t "~A~%" *ts*))

(defun find-local (name &key (scope (type-system-curscope *ts*)))
  (unless (< scope 0)
    (let ((loc (gethash name (lex-scope-locals (current-scope)))))
      (if loc
        loc
        (find-local name :scope (- scope 1))))))

(defun get-local-type (local)
  (when local (local-info-tid local)))

(defun get-node-kind (node)
  (first node))

(defun get-node-type (node)
  (second node))

(defun get-node-type-info (node)
  (destructuring-bind (scopeidx id) (second node)
    (gethash id (lex-scope-types (get-scope scopeidx)))))

(defun every-type (nodes)
  (every (lambda (it) (not (null (get-node-type it)))) nodes))

(defun type-symbol (id)
  (let* ((local (find-local id))
         (ltype (get-local-type local)))
    (cond
      ((and local ltype)    (list :symbol ltype id))
      (t                    (list :symbol nil id)))))

(defun type-intrinsic (fn args)
  (let* ((tfn           (get-node-type-info fn))
         (validators    (type-info-arg-validators tfn))
         (argtypes      (map 'list #'get-node-type args))
         (argtypeinfos  (map 'list #'get-node-type-info args))
         (argval        (mapcar #'list argtypeinfos validators))
         (args-match    (every (lambda (it) (funcall (second it) (first it))) argval)))
    (cond 
      (args-match       (list :intrinsic (first argtypes) (type-info-intrkind tfn) args))
      (t                (list :call nil fn args)))))

(defun type-call (fn args)
  (let* ((tfn               (type-node fn))
         (targs             (type-tree args))
         (all-resolved      (and (not (null (get-node-type tfn))) (every-type targs))))
    (format t "~A~%" (get-node-type tfn))
    (cond
      ((and all-resolved (is-type-kind (get-node-type-info tfn) :intrinsic)) (type-intrinsic tfn targs))
      (t                                                                     (list :call nil tfn targs)))))

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


(defmacro push-back (place lst)
  `(setf ,place (nconc ,lst ,place)))


;;; IR

(defparameter *ir-instrs* nil)
(defparameter *ir-vstack* (make-array 32))
(defparameter *ir-vtop* 0)

(defun ir-emit (irkind &rest args)
  (push-back *ir-instrs* (list (list irkind nil args))))

(defun ir-emit-typed (irkind typ &rest args)
  (push-back *ir-instrs* (list (list irkind typ args))))

(defun push-vstack (value)
  (setf (aref *ir-vstack* *ir-vtop*) value)
  (incf *ir-vtop*))

(defun pop-vstack ()
  (decf *ir-vtop*)
  (aref *ir-vstack* *ir-vtop*))

(defun is-arith-intrinsic (ikind)
  (some (lambda (it) (equal it ikind)) '(:add :sub :mul)))

(defun generate-ir-arith-intrinsic (typ ikind args)
  (progn
    (generate-ir-node (first args))
    (generate-ir-node (second args))
    (let* ((b (pop-vstack))
           (a (pop-vstack)))
      (ir-emit-typed ikind typ a b)
      (push-vstack (list :stack typ)))))

(defun generate-ir-intrinsic (node)
  (destructuring-bind (_ typ ikind args) node
    (declare (ignore _))
    (cond
      ((is-arith-intrinsic ikind)   (generate-ir-arith-intrinsic typ ikind args))
      (t                            nil))))

(defun generate-ir-node (node)
  (let ((k (get-node-kind node)))
    (cond
      ((equal k :intrinsic)    (generate-ir-intrinsic node))
      ((equal k :integer-lit)  (push-vstack node))
      ((equal k :float-lit)    (push-vstack node)))))

(defun generate-ir-instrs (ast)
  (dolist (node ast)
    (generate-ir-node node)))


;;; Codegen (x64)

(defun get-ir-arg (instr idx)
  (nth (last instr) idx))

(defun transform-ir-arg (arg)
  (case (first arg)
    (:integer-lit
      arg
    (:stack
      (pop-codegen-vstack)))))

(defun generate-arith-instr-assembly (instr)
  (let* ((b (transform-ir-arg (get-ir-arg instr 1))))
        ((a (transform-ir-arg (get-ir-arg instr 0))))
    (if (not (regp a))
      (progn
        (codegen-move (reg-dest +reg-intermediate+ res-type) a)
        (codegen-arith-op (first instr) (reg-dest +reg-intermediate+ res-type) b))
      (codegen-arith-op (first instr) a b))))

(defun generate-instr-assembly (instr)
  (if (is-arith-intrinsic (first instr))
    (generate-arith-instr-assembly instr)))

(defun generate-assembly (instrs)
  (dolist (instr instrs)
    (generate-instr-assembly instr)))


;;; Main

(defun compile-file-contents (src)
  (let* ((ast (parse-unit src))
         (semtree (ast-to-semtree (car ast))))
    (init-type-system)
    (format t "~A~%" ast)
    (format t "~A~%" semtree)
    (format t "~A~%" (type-tree semtree))
    (setf *ir-instrs* nil)
    (generate-ir-instrs (type-tree semtree))
    (setf *ir-instrs* (reverse *ir-instrs*))
    *ir-instrs*))