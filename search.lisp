(in-package #:kafgsk)


(defun recur-files (path)
  ;; получаем список файлов и поддиректорий
  (let ((files (directory (merge-pathnames path "*.*")))
        (dirs  (directory (merge-pathnames path "*"))))
    (setf files (remove-if #'(lambda (x)
                               (find x dirs :test #'(lambda (a b)
                                                      (string= (format nil "~A" a)
                                                               (format nil "~A" b)))))
                           files))
    ;; для каждой поддиректории:
    (loop :for dir :in dirs :do
       ;; рекурсивно вызываем себя
       (setf files (append files (recur-files dir))))
    ;; возвращаем результат
    files))

(defun get-all-pages (path)
  (let ((all-files (recur-files path)))
    (loop :for file :in all-files :collect
       (list (format nil "~A" file)
             (alexandria:read-file-into-string file)))))

(defun get-search-words (search-query)
  (split-sequence:split-sequence #\Space search-query))


(defun search-word-in-page (word page)
  (search word page :test #'string=))


(defun search-in-pages (search-query pages)
  ;; разбить поисковый запрос на слова
  (let ((results nil)
        (words (get-search-words search-query)))
    (loop :for word :in words :do
       ;; для каждого слова: взять все страницы
       (loop :for (file page) :in pages :do
          ;; для каждой страницы: искать слово
          (if (search-word-in-page word page)
              ;; если слово найдено - добавить в результаты
              (push file results))))
    (remove-duplicates results)))

(search-in-pages "факультет информационных технологий" (get-all-pages (path "content/")))

;; TODO
;; Приводить слово к нормальной форме (отрезать окончания)
;; Оценивать релевантность
;; Выводить сниппет

;; Релевантность (пример)

(setf word "для")
(setf str-one "предположим, для примера, что вы пишете программу для")
(setf str-two "для для для")
(setf str-three "это для тебя")
(setf all-strings (list str-one str-two str-three))

(let ((y 0))
(defun my-search (str)
  (let ((x (search word str)))
    (if (search word str)
        (progn
          (setf y (+ y 1))
          (my-search (subseq str (+ x (length word)))))
        y))))

(defun sort-strings (strings)
  (sort
   (mapcar #'my-search strings) #'>))

(sort-strings all-strings)

(trace my-search)

(my-search str-one)

