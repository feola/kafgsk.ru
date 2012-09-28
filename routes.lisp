(in-package #:kafgsk)



;; 404

(defun page-404 (&optional (title "404 Not Found") (content "Страница не найдена"))
  "404")

(restas:define-route not-found-route ("*any")
  (restas:abort-route-handler
   (page-404)
   :return-code hunchentoot:+http-not-found+
   :content-type "text/html"))

(defun old-page (filename)
  (tpl:root (list :content (alexandria:read-file-into-string (path filename)))))

;; main

(restas:define-route main ("/")
  (old-page "content/main.htm"))


;; plan file pages

(defmacro def/route (name param &body body)
  `(progn
     (restas:define-route ,name ,param
       ,@body)
     (restas:define-route
         ,(intern (concatenate 'string (symbol-name name) "/"))
         ,(cons (concatenate 'string (car param) "/") (cdr param))
       ,@body)))


(def/route 01.07.12 ("01.07.12")
  (old-page "content/01.07.12.htm"))

(def/route 01.09.12 ("01.09.12")
  (old-page "content/01.09.12.htm"))

(def/route 25.06.12 ("25.06.12")
  (old-page "content/25.06.12.htm"))

(def/route arhiv2012 ("arhiv2012")
  (old-page "content/arhiv2012.htm"))

(def/route arhiv2013 ("arhiv2013")
  (old-page "content/arhiv2013.htm"))

(def/route arhiv ("arhiv")
  (old-page "content/arhiv.htm"))

(def/route aspirantura ("aspirantura")
  (old-page "content/aspirantura.htm"))

(def/route contacts ("contacts")
  (old-page "content/contacts.htm"))

(def/route diplomy ("diplomy")
  (old-page "content/diplomy.htm"))


(def/route predmety ("predmety")
  (tpl:root (list :content (format nil "<br /> <ul> ~{ <br /><li> ~A </li> ~} </ul>"
                                   (loop :for (curs . curs-id) :in (all-curs) :collect
                                      (format nil "<a name=\"id~A\" href=\"/predmety/~A\">~A</a>"
                                              curs-id
                                              curs-id
                                              (name curs)))))))


;; (loop :for (curs . curs-id) :in (all-curs) :collect

;;(let ((curs (get-curs 1))
;;    (curs-id 1))

;; (let ((rs))
;; (loop :for (teacher . teacher-id) :in (all-teacher) :do
;;  (when (find curs-id (curses teacher))
;;  (push (format nil "<a href=\"/prepody#id~A\">~A</a>"
;;              teacher-id
;;            (name teacher))
;;  rs)))
;; rs))



(def/route predmety/id ("predmety/:curs-id")
  (let* ((curs-id (parse-integer curs-id :junk-allowed t))
         (curs (get-curs curs-id)))
    (tpl:root (list :content
                    (concatenate 'string
                                 (format nil "<br /> <br /> ~A <br /> <br /> Преподаватели: <br /> ~A ~A"
                                         (name curs)
                                         (let ((rs))
                                           (loop :for (teacher . teacher-id) :in (all-teacher) :do
                                              (when (find curs-id (curses teacher))
                                                (push (format nil "<a href=\"/prepody/~A\">~A</a>"
                                                              teacher-id
                                                              (name teacher))
                                                      rs)))
                                           (format nil "<ul> ~{ <li> ~A </li> ~} </ul>"
                                                   rs))
                                         (let ((filename (path (format nil "content/predmety/~A.htm" curs-id))))
                                           (if (probe-file filename)
                                               (alexandria:read-file-into-string filename)
                                               "warning: no file!"))))))))


(def/route prepody ("prepody")
  (tpl:root (list :content (format nil "<br /> <ul> ~{ <br /><li> ~A </li> ~} </ul>"
                                   (loop :for (teacher . teacher-id) :in (all-teacher) :collect
                                      (format nil "<a name=\"id~A\" href=\"/prepody/~A\">~A</a>"
                                              teacher-id
                                              teacher-id
                                              (name teacher)))))))


(def/route prepody/id ("prepody/:teacher-id")
  (let ((teacher (get-teacher (parse-integer teacher-id :junk-allowed t))))
    (tpl:root (list :content
                    (concatenate 'string
                                 (format nil "<br /> <br /> ~A <br /> <br /> Предметы: <br /> ~A "
                                         (name teacher)
                                         (format nil "<ul> ~{ <li> ~A </li> ~} </ul>"
                                                 (mapcar #'(lambda (x)
                                                             (format nil "<a href=\"/predmety/~A\">~A</a>"
                                                                     x
                                                                     (name (get-curs x))))
                                                         (curses teacher))))
                                 (let ((filename (path (format nil "content/prepody/~A.htm" teacher-id))))
                                   (if (probe-file filename)
                                       (alexandria:read-file-into-string filename)
                                       "warning: no file!")))))))


(def/route foto ("foto")
  (old-page "content/foto.htm"))

(def/route history ("history")
  (old-page "content/history.htm"))

(def/route index ("index")
  (old-page "content/index.htm"))

(def/route lab2009 ("lab2009")
  (old-page "content/lab2009.htm"))

(def/route praktika ("praktika")
  (old-page "content/praktika.htm"))

(def/route pr_doc ("pr_doc")
  (old-page "content/pr_doc.htm"))




(def/route pr_etap1 ("pr_etap1")
  (old-page "content/pr_etap1.htm"))

(def/route pr_etap2 ("pr_etap2")
  (old-page "content/pr_etap2.htm"))

(def/route pr_raspr2012 ("pr_raspr2012")
  (old-page "content/pr_raspr2012.htm"))

(def/route pr_spisorg ("pr_spisorg")
  (old-page "content/pr_spisorg.htm"))

(def/route pr_sroki ("pr_sroki")
  (old-page "content/pr_sroki.htm"))

(def/route pr_vidy ("pr_vidy")
  (old-page "content/pr_vidy.htm"))

(def/route pr_zayavka ("pr_zayavka")
  (old-page "content/pr_zayavka.htm"))

(def/route raspisanie ("raspisanie")
  (old-page "content/raspisanie.htm"))


;; submodules

(restas:mount-submodule -css- (#:restas.directory-publisher)
                        (restas.directory-publisher:*baseurl* '("css"))
                        (restas.directory-publisher:*directory* (path "css/")))

(restas:mount-submodule -js- (#:restas.directory-publisher)
                        (restas.directory-publisher:*baseurl* '("js"))
                        (restas.directory-publisher:*directory* (path "js/")))

(restas:mount-submodule -img- (#:restas.directory-publisher)
                        (restas.directory-publisher:*baseurl* '("img"))
                        (restas.directory-publisher:*directory* (path "img/")))

(restas:mount-submodule -resources- (#:restas.directory-publisher)
                        (restas.directory-publisher:*baseurl* '("resources"))
                        (restas.directory-publisher:*directory* (path "resources/")))
