(in-package #:kafgsk)

(defun menu ()
  (list (list :link "/" :title "Главная")
        (list :link "/about" :title "About")
        (list :link "/articles/" :title "Статьи")
        (list :link "/aliens/" :title "Материалы")
        (list :link "/resources/" :title "Ресурсы")
        (list :link "/faq/" :title "FAQ")
        ;; (list :link "/job/" :title "О, работа!")
        (list :link "/contacts" :title "Контакты")))


;; start
(restas:start '#:kafgsk :port 9993)
(restas:debug-mode-on)
;; (restas:debug-mode-off)
(setf hunchentoot:*catch-errors-p* t)
