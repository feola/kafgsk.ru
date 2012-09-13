(in-package #:kafgsk)

;; start
(restas:start '#:kafgsk #|:hostname "rigidus.ru"|# :port 9992)
(restas:debug-mode-on)
;; (restas:debug-mode-off)
(setf hunchentoot:*catch-errors-p* t)
