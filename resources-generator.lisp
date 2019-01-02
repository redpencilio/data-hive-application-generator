(in-package :resources-generator)

(defun generate-resources ()
  "Generate a string with a list of eg commands"
  (format t "~{~A~%~}"
          (mapcar #'generate-resource (all-resources))))

(defun generate-dispatchers ()
  "Generate a string with a list of dispatcher rules"
  (format t "~{~A~^ ~%~}"
          (mapcar #'generate-dispatcher (all-resources))))

(defun generate-prefixes ()
  "Returns an array containing all embedded prefixes"
  (format t "ember generate rdfa-prefixes \"~A\" ~{\"~{~A:~A\"~}~,^ ~}"
          "http://schema.org/"
          (loop for (prefix . uri) in (cl-fuseki:get-prefix-alist)
                collect `(,prefix ,uri))))

(defun all-resources ()
  "Fetch all resources from domain.lisp"
  (loop for val being
     the hash-values of mu-cl-resources::*resources*
     collect val))


(defun generate-resource (resource)
  "Generate a string with an eg command for one resource"
  (format nil "~A \"~A&~A\" ~{~A ~}~A~A ~[~;--readonly~]"
          "ember g mu-resource" ; terminal command
          (gen-resource-name resource)  ; resource name
          (mu-cl-resources::ld-class resource) ; resource type
          (mapcar #'gen-resource-slot (mu-cl-resources::ld-properties resource)) ; its attributes
          (gen-resource-has-one-links resource)
          (gen-resource-has-many-links resource)
          (readonly))) ; sets readonly flag for ever eg command if readonly env is set


(defun gen-resource-has-one-links (resource)
  (format nil "~{~A ~}"
          (loop for link in (mu-cl-resources::has-one-links resource)
             for resource-rel = (gen-resource-rel link resource)
             for property = (and (not (mu-cl-resources::inverse-p link))
                               (mu-cl-resources::ld-link link))
             collect
               (format nil "\"~{~A:belongs-to:~A~#[~;~~~A~]~}~@[&~A~]\""
                       resource-rel property))))

(defun gen-resource-has-many-links (resource)
  (format nil "~{~A ~}"
          (loop for link in (mu-cl-resources::has-many-links resource)
             for resource-rel = (gen-resource-rel link resource)
             for property = (and (not (mu-cl-resources::inverse-p link))
                               (mu-cl-resources::ld-link link))
             collect
               (format nil "\"~{~A:has-many:~A~#[~;~~~A~]~}~@[&~A~]\""
                       resource-rel property))))

(defun gen-resource-name (resource)
  (string-downcase (mu-cl-resources::resource-name resource)))

(defun gen-resource-slot (property)
  (format nil "\"~A:~A&~A\""
          (string-downcase (symbol-name (mu-cl-resources::json-key property)))
          (string-downcase (symbol-name (mu-cl-resources::resource-type property)))
          (mu-cl-resources::ld-property property)))

(defun gen-resource-rel (link resource)
  (let ((inverse-link (find-inverse link resource)))
    (if inverse-link
        `(,(mu-cl-resources::request-path link) ,(string-downcase (mu-cl-resources::resource-name link)) ,inverse-link)
        `(,(mu-cl-resources::request-path link) ,(string-downcase (mu-cl-resources::resource-name link))))))

(defun find-inverse (link resource)
  ;; NOTE: this method contains a workaround, as muclr:1.15.0 doesn't check whether
  ;;       inverse relations on a predicate actually point to each other.
  (let ((inverse-links (mu-cl-resources::inverse-links link)))
    (when (and
           inverse-links ; an inverse predicate exists
           (equalp ; and the relations on the predicate point to each other
            (mu-cl-resources::resource-name link)
            (mu-cl-resources::resource-name (getf (car inverse-links) :resource))))
      (string-downcase (mu-cl-resources::request-path (getf (car inverse-links) :link))))))

(defun readonly ()
  (if (env-value :readonly) 1 0))

(defun env-value (setting)
  "Returns the value of the supplied environment variable."
  (sb-ext:posix-getenv (string-upcase (string setting))))


;; Generate a string with a dispatcher rule for one resource
(defun generate-dispatcher (resource)
  "Returns the dispatcher rule for the specific resource"
  (let ((path (string-downcase (mu-cl-resources::request-path resource))))
    (format nil "  match \"/~A/*path\" do~%    Proxy.forward conn, path, \"http://resource/~A/\"~%  end"
            path
            path)))


