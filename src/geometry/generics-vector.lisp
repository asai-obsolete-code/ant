(in-package :report.geometry)
(speed*)
(annot:enable-annot-syntax)

@export
@doc "returns a resized VECTOR-OR-DIRECTIONABLE
 whose length is same as LENGTH"
(defgeneric resize (vector-or-directionable length))

@export
@doc "returns the sum of the vectors"
(defgeneric add (v1 v2))
@export
@doc "returns the difference of the vectors"
(defgeneric sub (v1 v2))

@export
@doc "returns the length of a vector"
(defun norm (v)
  @type 2dvector v
  @inline norm2
  (dsqrt (norm2 v)))

@export
@doc "returns the square of the length of a vector"
(defgeneric norm2 (v1))

@export
@doc "returns the scalar product of two vectors"
(defgeneric dot (v1 v2))

@export
@doc "returns a vector whose length is 1"
(defgeneric normalize (v1))
