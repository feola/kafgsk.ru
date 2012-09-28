(in-package #:kafgsk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; default-render
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass default-render () ())

(defparameter *default-render-method* (make-instance 'default-render))


(defmethod restas:render-object ((designer default-render) (data list))
  "fwefewf")
  ;; (destructuring-bind (headtitle navpoints content) data
  ;;   (tpl:root (list :headtitle headtitle
  ;;                   :content (tpl:base (list :navpoints navpoints
  ;;                                            :content content
  ;;                                            :stat (tpl:stat)))))))

(defmethod restas:render-object ((designer default-render) (file pathname))
  "org-wefewfewfw")
  ;; (if (string= (pathname-type file) "org")
  ;;     (restas:render-object designer (parse-org file))
  ;;     (call-next-method)))


