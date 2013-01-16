
(in-package :ant)
(annot:enable-annot-syntax)

(defun wrap-closure (fn-sim)
  (lambda (&rest args)
	(apply (symbol-function fn-sim) args)))

(defun main ()
  (gtk:within-main-loop
	(let ((window (make-instance 'gtk:gtk-window
								 :type :toplevel
								 :window-position :center
								 :default-width 400
								 :default-height 400
								 :keep-above t
								 :title "Ant Main window"))
		  (canvas (make-instance 'gtk:drawing-area
								 :default-width 300
								 :default-height 300))
		  (vbox (make-instance 'gtk:v-box)))
	  (gtk:container-add window vbox)
	  (gtk:box-pack-start vbox canvas)
	  (gtk:widget-show window)
	  (push :all-events-mask
			(gdk:gdk-window-events (gtk:widget-window canvas)))
	  (mapc
	   (lambda (pair)
		 (destructuring-bind (key fn-sym) pair
		   (gobject:connect-signal window key (wrap-closure fn-sym))))
	   '(("button-press-event" button-press)
		 ("button-release-event" button-release)
		 ("key-press-event" key-press)
		 ("key-release-event" key-release)
		 ("motion-notify-event" motion-notify)
		 ("scroll-event" scroll)))
	  (setup)
	  (gtk:gtk-main-add-timeout
	   100
	   (lambda ()
		 (reflesh canvas)
		 t)))))

(defun draw-in-context (canvas fn)
  (let ((drawable (gtk:widget-window canvas)))
	(cl-gtk2-cairo:with-gdk-context
		(ctx drawable)
	  (multiple-value-bind (width height)
		  (gdk:drawable-get-size drawable)
		(cairo:with-context (ctx)
		  (funcall fn width height))))))

@eval-always
(defmacro with-context ((&optional width height) canvas &body body)
  (once-only (canvas)
	(unless width (setf width (gensym)))
	(unless height (setf height (gensym)))
	`(draw-in-context
	  ,canvas
	  (lambda (,width ,height)
		(declare (ignorable ,width ,height))
		,@body))))

@eval-always
(defmacro with-saved-context (&body body)
  `(progn
	 (cairo:save cairo:*context*)
	 ,@body
	 (cairo:restore cairo:*context*)))

(defun reflesh (canvas)
  (with-context (w h) canvas
	(with-saved-context 
	  (cairo:set-source-rgba 0.8 0.8 0.8 1)
	  (cairo:set-operator :source)
	  (cairo:paint))
	  (with-saved-context
		(let ((factor (scaling-factor *scale*)))
		  (cairo:scale factor factor))
		(with-slots (x y) *translation*
		  (cairo:translate x y))

		(cairo:set-line-width 0.1)

		(cairo:rectangle 0 0 *width* *height*)
		(cairo:stroke)
		(cairo:set-source-rgb 1 0.3 0.3)
		(cairo:arc (+ 0.5 *colony-x*)
				   (+ 0.5 *colony-y*) 5 0 +2pi+)
		(cairo:stroke)

		;; draws field
		(with-iter-array (f x y) *field*
		  (cairo:rectangle x y 1 1)
		  (cairo:set-source-rgba
		   1 1 0.3 (/ (field-food f) *field-max-food*))
		  (cairo:fill-path)
		  
		  (cairo:rectangle x y 1 1)
		  (cairo:set-source-rgba
		   0.3 0.3 1 (/ (field-pheromon f) *field-max-pheromon*))
		  (cairo:fill-path))

		;; draws ants
		(dolist (ant *ants*)
		  (with-slots (x y) ant
			(cairo:set-source-rgb 0.1 0.1 0.1)
			(cairo:rectangle x y 1 1)
			(cairo:fill-path)
			
			(cairo:set-source-rgb 1 0.3 0.3)
			;; (/ (ant-food ant) *ant-max-food*)
			(cairo:rectangle x y 1 1)
			(cairo:stroke))))))
