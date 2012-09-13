(in-package #:kafgsk)

;; macro-utils

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)  (pprint ,var)) 1))

(defmacro err (var)
  `(error (format nil "ERR:[~A]" (bprint ,var))))


;; data

(defparameter *teachers-list*
  '(("Моргунов Константин Петрович" "доцент, к.т.н"
     ("Улучшение качества воды" "Основы гидравлики и теплотехники" "Гидравлика" "Гидромеханика"))

    ("Гапеев Анатолий Михайлович" "профессор, д.т.н"
     ("ГЭС и гидромашины" "ГТС К и ОН"))

    ("Колосов Михаил Алесандрович" "профессор, д.т.н."
     ("Исследование, эксплуатация и ремонт ГТС" "Безопасность ГТС" "Безопасность в/х систем"
      "В/х и основы в/х проектирования"))

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
     ("Восстановление рек и водоёмов" "Инженерные системы водоснабжения"
      "Инженерные системы водоотведения"))

    ("Ушакевич Александр Николаевич" "ст. преподаватель"
     ("Гидрофизика" "Гидравлика" "Гидравлика ГТС"))
    ))

;; expanders

(let ((inc-curs-id 0))
  (defun incf-curs-id () (incf inc-curs-id))
  (defun init-curs-id (init-value) (setf inc-curs-id init-value))
  (defparameter *curs* (make-hash-table :test #'equal))
  (defun count-curs () (hash-table-count *curs*))
  (defclass curs ()
    ((name :initarg :name :initform "" :accessor name)))
  (defun make-curs (&rest initargs)
    (let ((id (incf-curs-id)))
      (values
       (setf (gethash id *curs*)
             (apply #'make-instance (list* 'curs initargs)))
       id)))
  (defun all-curs ()
    (loop :for v :being :the :hash-values :in *curs* :using (hash-key k) :collect
       (cons v k)))
  (defun get-curs (var)
    (when (typep var 'integer)
      (multiple-value-bind (hash-val present-p)
          (gethash var *curs*)
        (unless present-p (err 'not-present))
        (setf var hash-val)))
    (unless (typep var 'curs)
      (err 'param-user-type-error))
    var)
  (defmethod find-curs ((obj curs))
    (loop :for v :being :the :hash-values :in *curs* :using (hash-key k)
       :do (when (equal v obj) (return k))))
  (defmethod find-curs ((func function))
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
  (defclass teacher ()
    ((name   :initarg :name   :initform ""  :accessor name)
     (rank   :initarg :rank   :initform ""  :accessor rank)
     (curses :initarg :curses :initform nil :accessor curses)))
  (defun make-teacher (&rest initargs)
    (let ((id (incf-teacher-id)))
      (values
       (setf (gethash id *teacher*)
             (apply #'make-instance (list* 'teacher initargs)))
       id)))
  (defun all-teacher ()
    (loop :for v :being :the :hash-values :in *teacher* :using (hash-key k) :collect
       (cons v k)))
  (defun get-teacher (var)
    (when (typep var 'integer)
      (multiple-value-bind (hash-val present-p)
          (gethash var *teacher*)
        (unless present-p (err 'not-present))
        (setf var hash-val)))
    (unless (typep var 'teacher)
      (err 'param-user-type-error))
    var)
  (defmethod find-teacher ((obj teacher))
    (loop :for v :being :the :hash-values :in *teacher* :using (hash-key k)
       :do (when (equal v obj) (return k))))
  (defmethod find-teacher ((func function))
    (let ((rs))
      (mapcar
       #'(lambda (x)
           (if (funcall func x)
               (push x rs)))
       (all-teacher))
      rs)))


;; Создаем объекты учителей и курсов (см. в консоли лог создания и связывания объектов)
(loop :for teacher-elt :in *teachers-list* :do
   (let ((teacher-pair (multiple-value-bind (obj id)
                           (make-teacher :name (car teacher-elt)  :rank (cadr teacher-elt))
                         (cons obj id))))
     (format t "~%=== make-teacher ~3D : ~A" (cdr teacher-pair) (name (car teacher-pair)))
     ;; Для каждого из названий курсов, которые ведет этот учитель...
     (loop :for curs :in (caddr teacher-elt) :do
        ;; Попробуем поискать среди уже созданных курсов
        (let ((curs-pair (car (find-curs #'(lambda (x) (string= (name (car x)) curs))))))
          (when curs-pair
            ;; Нашли среди созданных курсов курс с таким же названием
            (format t "~%::: find-kurs    ~3D    : ~A" (cdr curs-pair) (name (car curs-pair))))
          (unless curs-pair
            ;; Если курса с таким названием еще нет то создаем объект курса
            (multiple-value-bind (obj id)
                (make-curs :name curs)
            (setf curs-pair (cons obj id))
            (format t "~%--- make-kurs    ~3D    : ~A" (cdr curs-pair) (name (car curs-pair)))))
          ;; Добавляем связь с курсом в поле `curses` учителя
          (push (cdr curs-pair) (curses (car teacher-pair)))))))


;; Получаем все курсы
(all-curs)

;; Получаем всех учителей
(all-teacher)

;; Получаем названия всех курсов
(mapcar #'(lambda (x)
            (print (name (car x))))
        (all-curs))

;; Находим объект курса по имени
(find-curs #'(lambda (x)
               (string= (name (car x)) "Безопасность ГТС")))


;; Получаем имена учителей вместе с номерами курсов которые они ведут
(mapcar #'(lambda (x)
            (print (list (name (car x)) (curses (car x)))))
        (all-teacher))

