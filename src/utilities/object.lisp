

(in-package :report.utilities)
(annot:enable-annot-syntax)

@export
(defclass recursive-print-mixin () ())

(defmethod print-object ((o recursive-print-mixin) stream)
  (print-unreadable-object (o stream :type t)
	(format stream
			"~{~{~<~a~>: ~<~a~> ~}~^~_~}"
			(mapcar (lambda (name)
					  (list name
							(if (slot-boundp o name)
								(slot-value o name)
								:unbound)))
					(mapcar #'slot-definition-name
							(class-slots
							 (class-of o)))))))

@export
(defgeneric shallow-copy (obj &rest args &key &allow-other-keys))
(defmethod shallow-copy ((n number) &rest args &key &allow-other-keys)
  n)
(defmethod shallow-copy ((s sequence) &rest args &key &allow-other-keys)
  (copy-seq s))
(defmethod shallow-copy ((s array) &rest args &key &allow-other-keys)
  (copy-array s))
(defmethod shallow-copy ((s hash-table) &rest args &key &allow-other-keys)
  (copy-hash-table s))
(defmethod shallow-copy ((o standard-object) &rest args
						 &key &allow-other-keys)
  (let* ((class (class-of o))
		 (new (allocate-instance class)))
	(mapc
	 (lambda (name)
	   (ignore-errors
		 (setf (slot-value new name)
			   (slot-value o name))))
	 (mapcar #'slot-definition-name
			 (class-slots class)))
	(apply #'reinitialize-instance new args)
	new))

;; @export
;; (defgeneric deep-copy (obj))
;; (defmethod defmethod (o)
;;   (%deep-copy-std o nil))

;; (defun %deep-copy-std (o lst)
;;   (let* ((class (class-of o))
;; 		 (new (make-instance class)))
;; 	(mapcar
;; 	 (lambda (name)
;; 	   (ignore-errors
;; 		 (let ((old-slot-value (slot-value o name)))
;; 		   (setf (slot-value new name)
;; 				 (deep-copy old-slot-value))
;; 		   old-slot-value)))
;; 	 (mapcar #'slot-definition-name
;; 			 (class-slots class)))
;; 	new)
