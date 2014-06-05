
(in-package :report.geometry)
(annot:enable-annot-syntax)

@export
@export-slots
(defclass 3dplane (infinity-shape 3dshape)
  ((base :type 3dvector
         :initarg :base
         :initform (3dv-coerce 0 0 0)
         :accessor base-of)
   (normal :type 3dvector
           :initarg :normal
           :initform (3dv-coerce 0 0 1)
           :reader normal-of)))

(defmethod implicit-function ((plane 3dplane))
  (lambda (p)
    @type 3dvector p
    (with-slots (base normal) plane
      (dot normal (sub p base)))))

(defmethod parallel-p ((pl 3dplane) (v vector))
  (zerop (dot (normal-of pl) v)))
(defmethod parallel-p ((v vector) (pl 3dplane))
  (zerop (dot (normal-of pl) v)))

(defmethod parallel-p ((p1 3dplane) (p2 3dplane))
  (zerop (norm (vector-prod (normal-of p1)
                            (normal-of p2)))))

(defmethod congruent-p ((p1 3dplane) (p2 3dplane))
  t)

;; (defmethod congruent-p ((p1 3dplane) (p2 3dplane))
;;   (and (parallel-p p1 p2)
;; 	   (perpendicular-p (normal-of p1)
;; 						(sub (base-of p1)
;; 							 (base-of p2)))))

(defmethod perpendicular-p ((pl 3dplane) (v vector))
  (zerop (norm (vector-prod (normal-of pl) v))))
(defmethod perpendicular-p ((v vector) (pl 3dplane))
  (zerop (norm (vector-prod (normal-of pl) v))))
(defmethod perpendicular-p ((p1 3dplane) (p2 3dplane))
  (zerop (dot (normal-of p1)
              (normal-of p2))))

