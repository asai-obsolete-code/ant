
(in-package :report.utilities)
(annot:enable-annot-syntax)

@export
@doc "gaussian random algorithm with box-muller method."
(defun gaussian-drandom (&optional (μ1 0.0d0) (σ1 1.0d0) μ2 σ2)
  (let* ((x (drandom 1.0d0))
		 (y (drandom 1.0d0))
		 (_x (dsqrt (d- (d* 2.0d0 (dlog x))))))
	(values (d+ μ1 (d* σ1 _x (dcos (d* +2pi+ y))))
			(d+ (or μ2 μ1) (d* (or σ2 σ1) _x (dsin (d* +2pi+ y)))))))


@eval-always
@export
(defmacro bias-if (bias then &optional else)
  `(if (d< (drandom 1.0d0) ,bias)
	   ,then
	   ,else))


@eval-always
@export
(defmacro bias-cond (&body clauses)
  (iter (for clause in clauses)
		(collect (first clause) into rates)
		(collect (rest clause) into bodies)
		(finally
		 (return
		   `(call-bias-cond
			 (list ,@rates)
			 (list ,@(mapcar
					  (lambda (body)
						`(lambda ()
						   ,@body))
					  bodies)))))))

;; (bias-cond
;;  ((+ 1 2) (dosomething))     ; random rate is 3 : 5 : 15
;;  (5       (dosomething))     ;
;;  ((* 3 5) (do-other-thing))) ;

@export
(defun make-upsteer (lst)
  (iter (for n in lst)
		(summing n into sum)
		(collect sum at beginning)))

(defun call-bias-cond (rate-list fn-list)
  (let ((steer (make-upsteer rate-list)))
	(funcall (nth (position (random (car steer))
							(nreverse steer)
							:test #'<)
				  fn-list))))


@export
(defun drandom-between (x0 x1)
  (d+ x0 (drandom (d- x1 x0))))

@export
(defun random-between (x0 x1)
  (+ x0 (random (- x1 x0))))