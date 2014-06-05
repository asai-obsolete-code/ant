(in-package :report.geometry)
(annot:enable-annot-syntax)

(optimize*)

@export @export-accessors @doc "This is a mixin which stores the
motion of an object. Slot TIME, inherited from `time-mixin',
represents the current absolute time of the object. Slot MOTION, an
instance of `2+1dvector', represents planned absolute center point of
an object in the future. TIME slot in MOTION is also an absolute
time."
(defclass motion-mixin (time-mixin)
  ((motion :type 2+1dvector 
           :initarg :motion
           :accessor motion)
   (dt :type *desired-type*)
   (velocity :type 2dvector)))

(defmethod (setf motion) :after ((v 2+1dvector) (m motion-mixin))
  (slot-makunbound m 'velocity)
  (slot-makunbound m 'dt))

@export
(defun dt (movable)
  (with-memoising-slot (dt movable)
    (with-slots (motion time) movable
      (handler-case
          (d- (t-of motion) time)
        (FLOATING-POINT-OVERFLOW (c)
          @ignore c
          MOST-POSITIVE-DOUBLE-FLOAT)))))

(defmethod (setf velocity-of) ((v 2dvector) (m motion-mixin))
  (with-slots (motion) m
    (let ((vt (nadd-vector (scale-vector v (dt m))
                           (center-of m))))
      (setf (x-of motion) (x-of vt)
            (y-of motion) (y-of vt)))))

(defmethod velocity-of ((movable motion-mixin))
  (with-memoising-slot (velocity movable)
    (with-slots (motion) movable
      (nscale-vector (sub motion (center-of movable))
                     (d/ (dt movable))))))

;; 同様に、ボトルネックはどんどんメモ化していけばいい

;;全部同時刻を基準に考えているのを直さないといけない

@export
(defun time-range (movable)
  (make-range (t-of movable)
              (t-of (motion movable))))

@export
(defun sharing-time-range (movable1 movable2)
  (region-product (time-range movable1)
                  (time-range movable2)))

@export
(defun compute-position (movable time)
  (nadd-vector (scale-vector (velocity-of movable)
                             (d- time (t-of movable)))
               (center-of movable)))


@export
@doc "circular boundaryが接触する時間の範囲, for no range returns nil"
(defun time-range-of-circular-intersection (movable1 movable2)
  (when-let ((t-range (sharing-time-range movable1 movable2)))
    (let* ((time (range-from t-range))
           (dp (nsub-vector (compute-position movable1 time)
                            (compute-position movable2 time)))
           (dv (sub-vector (velocity-of movable2)
                           (velocity-of movable1)))
           (dv1 (normalize dv))
           (perp (dot dp (rotate90 dv1)))
           (r (d+ (radius movable1) (radius movable2))))
      (if (d< (dabs r) (dabs perp))
          nil
          (let* ((dvnorm (norm dv))
                 (t-center  (d+ time (d/ (dot dp dv1) dvnorm)))
                 (t-width/2 (d/ (dsqrt (d- (d* r r) (d* perp perp)))
                                dvnorm)))
            (region-product
             t-range
             (make-range
              (d- t-center t-width/2)
              (d+ t-center t-width/2))))))))

@export
(defun future-pos (m time)
  (translate m (scale-vector (velocity-of m) (d- time (t-of m)))))

@export
(defun intersect-at-time (m1 m2 time)
  (intersects-p
   (future-pos m1 time)
   (future-pos m2 time)))

(defun %when-intersect-last (m1 m2 dl)
  (iter
   (while dl)
   (for time = (dlist:dlist-pop dl))
   (when (intersect-at-time m1 m2 time)
     (return time))
   (finally
    (return nil))))

(defun %when-intersect (m1 m2 dl dt)
  (if (d< dt 1.0d-3)
      (%when-intersect-last m1 m2 dl)
      (iter
       (with next-dl = nil)
       (with next-dt = (d* 0.5d0 dt))
       (while dl)
       (for time = (dlist:dlist-pop dl))
       (when (intersect-at-time m1 m2 time)
         (return
           (%when-intersect
            m1 m2 (dlist:dlist (d- time next-dt)) next-dt)))
       (dlist:dlist-push (d- time next-dt) next-dl :at-end t)
       (dlist:dlist-push (d+ time next-dt) next-dl :at-end t)
       (finally
        (return
          (%when-intersect m1 m2 next-dl next-dt))))))

@export
@doc "Returns the remaining time span that those two `movable-mixin's will
 collide. if no collision was detected, return nil."
(defun when-intersect (m1 m2)
  (when-let ((time-range (time-range-of-circular-intersection m1 m2)))
    (with-accessors ((from range-from) (to range-to)) time-range
      (cond
        ((intersect-at-time m1 m2 from)
         from)
        ((intersect-at-time m1 m2 to)
         (%when-intersect
          m1 m2
          (dlist:dlist (d+ (center-of time-range) (radius time-range)))
          (radius time-range)))
        (t
         (%when-intersect
          m1 m2
          (dlist:dlist (center-of time-range))
          (diameter time-range)))))))

;; ;; ex. n=4, 2^4=16
;; ;; 16 :first
;; ;; 16でぶつかってる>手前に

;; 0  ; 0でぶつかってる -> そこが解
;; 8  ; 8

;; ;; 8で
;; ;; ぶつかってる   -> 手前に移動 4へ (-4)
;; ;; ぶつかってない -> 後ろと手前両方調べる 4と12へ 先に4

;; 4  ; 4 ;; ぶつかってる --> 2へ移動 ぶつかってない-> 2,6を追加して次のキューへ
;; 12 ; 4+8 ;; ぶつかってる --> 10へ移動 ぶつかってない -> 10,14を追加して次のキューへ

;; 2  ; 2
;; 6  ; 2+4
;; 10 ; 2+4+4
;; 14 ; 2+4+4+4
