(in-package #:kafgsk)

;; macro-utils

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)  (pprint ,var)) 1))

(defmacro err (var)
  `(error (format nil "ERR:[~A]" (bprint ,var))))

(defmacro do-hash ((ht &optional (v 'v) (k 'k)) &body body)
  `(loop :for ,v :being :the :hash-values :in ,ht :using (hash-key ,k) :do
      ,@body))

(defmacro do-hash-collect ((ht &optional (v 'v) (k 'k)) &body body)
  `(loop :for ,v :being :the :hash-values :in ,ht :using (hash-key ,k) :collect
      ,@body))

(defmacro append-link (lst elt)
  `(setf ,lst (remove-duplicates (append ,lst (list ,elt)))))

;; data

(defparameter *teachers-list*
  '(("Моргунов Константин Петрович" "доцент, к.т.н"
     ("Улучшение качества воды" "Основы гидравлики и теплотехники" "Гидравлика" "Гидромеханика"))

    ("Гапеев Анатолий Михайлович" "профессор, д.т.н"
     ("ГЭС и гидромашины" "ГТС К и ОН"))

    ("Колосов Михаил Алесандрович" "профессор, д.т.н."
     ("Исследование, эксплуатация и ремонт ГТС" "Безопасность ГТС" "Безопасность в/х систем" "В/х и основы в/х проектирования"))

    ("Гарибин Павел Андреевич" "профессор, д.т.н"
     ("ВВП и ГТС" "ГТС ВП, П и КШ"))

    ("Головков Сергей Анатольевич" "доцент, к.т.н"
     ("Обустройство в/х объектов" "ЖБК"))

    ("Кудрявцев Анатолий Валентинович" "доцент, к.т.н."
     ("Гидравлика"))

    ("Рябов Георгий Георгиевич" "ст. преподаватель"
     ("Речные ГТС" "САПР"))

    ("Ладенко Светлана Юрьевна" "доцен, к.т.н"
     ("Инженерная мелиорация" "Мелиорация водосборов"))

    ("Федотова Олеся Андреевна" "ст. преподаватель"
     ("Восстановление рек и водоёмов" "Инженерные системы водоснабжения" "Инженерные системы водоотведения"))

    ("Ушакевич Александр Николаевич" "ст. преподаватель"
     ("Гидрофизика" "Гидравлика" "Гидравлика ГТС"))
    ))

;; expanders

(let ((inc-curs-id 0))
  (defun incf-curs-id () (incf inc-curs-id))
  (defun init-curs-id (init-value) (setf inc-curs-id init-value))
  (defparameter *curs* (make-hash-table :test #'equal))
  (defun count-curs () (hash-table-count *curs*))
  (defclass curs nil ((name :initarg :name :initform "" :accessor name)))
  (defun make-curs (&rest initargs)
    (let ((id (incf-curs-id)))
      (values
       (setf (gethash id *curs*)
             (apply #'make-instance (list* 'curs initargs)))
       id)))
  (defun all-curs () (do-hash-collect (*curs*) (cons v k)))
  (defun get-curs (var)
    (when (typep var 'integer)
      (multiple-value-bind (hash-val present-p)
          (gethash var *curs*)
        (unless present-p (err 'not-present))
        (setf var hash-val)))
    (unless (typep var 'curs) (err 'param-user-type-error))
    var)
  (defmethod find-curs
      ((obj curs))
    (do-hash (*curs*) (when (equal v obj) (return k))))
  (defmethod find-curs
      ((func function))
    (let ((rs))
      (mapcar
       #'(lambda (x)
           (if (funcall func x)
               (push x rs)))
       (all-curs))
      rs)))

(let ((inc-teacher-id 0))
  (defun incf-teacher-id () (incf inc-teacher-id))
  (defun init-teacher-id (init-value) (setf inc-teacher-id init-value))
  (defparameter *teacher* (make-hash-table :test #'equal))
  (defun count-teacher () (hash-table-count *teacher*))
  (defclass teacher nil
    ((name :initarg :name :initform "" :accessor name)
     (rank :initarg :rank :initform "" :accessor rank)))
  (defun make-teacher (&rest initargs)
    (let ((id (incf-teacher-id)))
      (values
       (setf (gethash id *teacher*)
             (apply #'make-instance (list* 'teacher initargs)))
       id)))
  (defun all-teacher () (do-hash-collect (*teacher*) (cons v k)))
  (defun get-teacher (var)
    (when (typep var 'integer)
      (multiple-value-bind (hash-val present-p)
          (gethash var *teacher*)
        (unless present-p (err 'not-present))
        (setf var hash-val)))
    (unless (typep var 'teacher) (err 'param-user-type-error))
    var)
  (defmethod find-teacher
      ((obj teacher))
    (do-hash (*teacher*) (when (equal v obj) (return k))))
  (defmethod find-teacher
      ((func function))
    (let ((rs))
      (mapcar
       #'(lambda (x)
           (if (funcall func x)
               (push x rs)))
       (all-teacher))
      rs)))


;; Создаем объекты учителей и учеников
(loop :for x :in *teachers-list* :do
   (make-teacher :name (car x)  :rank (cadr x))
   ;; Для каждого из названий курсов, которые ведет этот учитель...
   (loop :for curs :in (caddr x) :do
      (make-curs :name curs)))

(all-curs)
(all-teacher)

(mapcar #'(lambda (x)
            (print (name (car x))))
        (all-curs))
