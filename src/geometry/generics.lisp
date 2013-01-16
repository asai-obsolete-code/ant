
(in-package :report.geometry)
(annot:enable-annot-syntax)

;; @doc "returns a function that takes 1 2dvector argument and returns a number which should be 0 when the vector is in the shape."

@export
@doc "rotates a shape. if the second argument was an instance of `2dmatrix', then
it will use that instance for rotating it."
(defgeneric rotate (shape rad))

@export
@doc "scale the SHAPE from the origin, by NUMBER."
(defgeneric scale (shape number))

@export
@doc "translate the SHAPE by NUMBER."
(defgeneric translate (shape vector))


@export
(defgeneric contains-p (shape vector)
  (:documentation "returns t if the SHAPE contains the
 VECTOR. otherwise return nil."))

@export
@doc "returns the area/volume of a shape."
(defgeneric volume (v))

@export
(defgeneric distance (a b)
  (:documentation "caliculates the distance between two objects.
if either of the arguments is spacious, then it will use the position which 
represents the object."))

@export
(defgeneric minimum-distance (a b)
  (:documentation "caliculates the minimum distance between two objects.
It will find the closest pair of points between the objects."))


@export
(defgeneric diameter (shape)
  (:documentation
   "find the distance between two farthest points in the shape."))

@export
(defgeneric radius (shape)
  (:documentation
   "return the radius of the shape, which is exactly the half of the diameter."))

@export
(defgeneric direction (directionable)
  (:documentation "find the direction of a line-like shape, including 
 `2dline', `2dsegment', `cone', `cylinder', etc."))

@export
(defgeneric direction-1 (directionable)
  (:documentation "find the direction of a line-like shape, including
 `2dline', `2dsegment', `cone', `cylinder', etc. the difference between
 direction and direction1 is that direction1 returns a normalized
 vector."))

@export
(defgeneric parallel-p (shape1 shape2)
  (:documentation "returns t if the two shapes are considered parallel."))

@export
(defgeneric perpendicular-p (shape1 shape2)
  (:documentation "returns t if the two shapes are considered perpendicular."))


@export
(defgeneric intersects-p (shape1 shape2)
  (:documentation "returns t if the two shapes are intersecting each other."))

@export
(defgeneric dimension (shape)
  (:documentation
   "gives the dimension of a particular SHAPE.
position is not included in the information.
return type is `2dvector'. for example,
the dimension of #<2DRECTANGLE [1,1] [3,4]> is #<2DVECTOR x:2 y:3>"))

@export
(defgeneric (setf dimension) (dimension shape))


@export
@doc "gives the minimum bounding rectangle (convex hull)
 of a particular SHAPE. position is included in the information.
Example:
the boundary of #<2DRECTANGLE [1,1] [3,4]> is #<2DRECTANGLE [1,1] [3,4]>"
(defgeneric boundary (shape))

@export
@doc "returns a range object which holds the x range of a SHAPE"
(defgeneric x-range-of (shape))
@export
@doc "returns a range object which holds the y range of a SHAPE"
(defgeneric y-range-of (shape))
@export
@doc "returns a range object which holds the y range of a SHAPE"
(defgeneric z-range-of (shape))

@export
@doc "returns the top-left point of a SHAPE"
(defgeneric top-left-of (shape))

@export
@doc "returns the bottom-right point of a SHAPE"
(defgeneric bottom-right-of (shape))

@export
@doc "returns the top-right point of a SHAPE"
(defgeneric top-left-of (shape))

@export
@doc "returns the bottom-left point of a SHAPE"
(defgeneric bottom-right-of (shape))

@export
@doc "returns if the two shapes are congruent."
(defgeneric congruent-p (shape1 shape2))


@export
@doc "convert something into a list"
(defgeneric ->list (shape))


@export
@doc "gives the projection of SHAPE to TO."
(defgeneric projection-of (shape to))

@export
@doc "gives the angle of 2DSHAPE from x-axis, in radian, clockwise."
(defgeneric angle (2dshape))

@export
@doc "gives the slope of 2DSHAPE."
(defgeneric slope (2dshape))

@export
@doc "gives the absolute position of the center of the SHAPE."
(defgeneric center-of (shape))

@export
@doc "returns a list of vertices"
(defgeneric vertices-of (shape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; minkowski- stuffs

@export
@doc "returns minkowski sum of two shapes"
(defgeneric minkowski-sum (shape1 shape2))

@export
@doc "returns minkowski difference of two shapes"
(defgeneric minkowski-difference (shape1 shape2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; region stuffs

@export
@doc "returns logical sum of two regions"
(defgeneric region-sum (shape1 shape2))

@export
@doc "returns logical difference of two regions"
(defgeneric region-difference (shape1 shape2))

@export
@doc "returns logical product of two regions"
(defgeneric region-product (shape1 shape2))

