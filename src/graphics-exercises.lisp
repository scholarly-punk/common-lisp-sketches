(defpackage :scholarly-sketch
  (:use :cl :sketch))

(in-package :scholarly-sketch)

(deftype non-minus-integer ()
  "A non-negative integer."
  '(integer 0 *))

(defstruct point-2d
  "Represents 2 dimensional coordinates."
  (x 0 :type non-minus-integer)
  (y 0 :type non-minus-integer))

(defclass line ()
  ((point-a :initarg :point-a :accessor line-point-a)
   (point-b :initarg :point-b :accessor line-point-b)
   (colour  :initarg :colour  :accessor line-colour))
  (:documentation "A geoemetric line"))

(defgeneric line-p (obj)
  (:documentation "A predicate for the line class.")
  (:method ((obj line)) t)
  (:method (obj) nil))

(defun make-line (&key (point-a) (point-b) (colour))
  (make-instance 'line
		 :point-a point-a
		 :point-b point-b
		 :colour colour))

(defmethod print-object ((object line) stream)
  (format stream "#<LINE (point-a: ~A) (point-b: ~A) (colour: ~A)>"
	  (line-point-a object)
	  (line-point-b object)
	  (line-colour object)))

(defgeneric draw-shape (a-shape)
  (:documentation "Draws a shape"))

(defmethod draw-shape ((a-shape line))
  (with-pen (make-pen :stroke (line-colour a-shape) :weight 3)
    (line (point-2d-x (line-point-a a-shape))
	  (point-2d-y (line-point-a a-shape))
	  (point-2d-x (line-point-b a-shape))
	  (point-2d-y (line-point-b a-shape)))))

(defclass solid-rectangle ()
  ((upper-left-corner :initarg :upper-left-corner :accessor solid-rectangle-upper-left-corner)
   (            width :initarg :width             :accessor solid-rectangle-width)
   (           height :initarg :height            :accessor solid-rectangle-height)
   (           colour :initarg :colour            :accessor solid-rectangle-colour)))

(defgeneric solid-rectangle-p (obj)
  (:documentation "A predicate for the rectangle class")
  (:method ((obj solid-rectangle)) t)
  (:method (obj) nil))

(defun make-solid-rectangle (&key (upper-left-corner) (width) (height) (colour))
  (make-instance 'solid-rectangle
		 :upper-left-corner upper-left-corner
		 :width width
		 :height height
		 :colour colour))

(defmethod draw-shape ((a-shape solid-rectangle))
  (with-pen (make-pen :fill (solid-rectangle-colour a-shape))
    (rect (point-2d-x (solid-rectangle-upper-left-corner a-shape))
	  (point-2d-y (solid-rectangle-upper-left-corner a-shape))
	  (solid-rectangle-width a-shape)
	  (solid-rectangle-height a-shape))))

(defmethod print-object ((object solid-rectangle) stream)
  (format stream "#<RECTANGLE (upper-left-corner: ~A) (width: ~A) (height: ~A) (colour: ~A)>"
	  (solid-rectangle-upper-left-corner object)
	  (solid-rectangle-width object)
	  (solid-rectangle-height object)
	  (solid-rectangle-colour object)))

(defclass solid-disk ()
  ((center :initarg :center :accessor solid-disk-center)
   (radius :initarg :radius :accessor solid-disk-radius)
   (colour :initarg :colour :accessor solid-disk-colour)))

(defgeneric solid-disk-p (obj)
  (:documentation "A predicate for the solid disk class.")
  (:method ((obj solid-disk)) t)
  (:method (obj) nil))

(defun make-solid-disk (&key (center) (radius) (colour))
  (make-instance 'solid-disk
		 :center center
		 :radius radius
		 :colour colour))

 (defmethod draw-shape ((a-shape solid-disk))
  (with-pen (make-pen :fill (solid-disk-colour a-shape))
    (circle (point-2d-x (solid-disk-center a-shape))
	    (point-2d-y (solid-disk-center a-shape))
	    (solid-disk-radius a-shape))))

(defclass circle ()
  ((center :initarg :center :accessor circle-center)
   (radius :initarg :radius :accessor circle-radius)
   (colour :initarg :colour :accessor circle-colour)))

(defgeneric circle-p (obj)
  (:documentation "A predicate for the circle class.")
  (:method ((obj circle)) t)
  (:method (obj) nil))

(defun make-circle (&key (center) (radius) (colour))
  (make-instance 'circle
		 :center center
		 :radius radius
		 :colour colour))

(defmethod draw-shape ((a-shape circle))
  (with-pen (make-pen :stroke (circle-colour a-shape) :fill +black+)
    (circle (point-2d-x (circle-center a-shape))
	    (point-2d-y (circle-center a-shape))
	    (circle-radius a-shape))))

(defsketch scholarly-graphics
    ((title "scholarly-graphics")
     (width 800)
     (height 800))
  (background +black+)

  (draw-shape (make-line :point-a (make-point-2d :x 45 :y 90)
			 :point-b (make-point-2d :x 145 :y 90)
			 :colour (hex-to-color "#98FB98")))
  
  (draw-shape (make-solid-rectangle :upper-left-corner (make-point-2d :x 100 :y 400)
				    :width 100
				    :height 80
				    :colour (hex-to-color "#FFF44F")))
  
  (draw-shape (make-solid-disk :center (make-point-2d :x 500 :y 500)
			       :radius 80
			       :colour (hex-to-color "#E3242B")))

  (draw-shape (make-circle :center (make-point-2d :x 700 :y 300)
			   :radius 60
			   :colour (hex-to-color "#95C8D8"))))

;; Draw the light with the red bulb turned on
(defsketch traffic-light-sketch
    ((title "traffic-light")
     (width 100)
     (height 160)
     (bulb-radius 20)
     (bulb-distance 10)
     (x-bulbs (truncate width 2))
     (y-red (+ bulb-distance bulb-radius))
     (y-yellow (+ y-red bulb-distance (* 2 bulb-radius)))
     (y-green (+ y-yellow bulb-distance (* 2 bulb-radius)))
     (current-colour +red+))
  (labels ((clear-bulb (light-colour)
	     "Turns off the light indicated by the symbol 'green, 'yellow or 'red."
	     (draw-shape
	      (make-circle :center (make-point-2d :x x-bulbs
						  :y (cond ((eq light-colour +red+) y-red)
							   ((eq light-colour +yellow+) y-yellow)
							   ((eq light-colour +green+) y-green)))
			   :radius bulb-radius
			   :colour light-colour)))

	   (draw-bulb (light-colour)
	     "Turns on the light indicated by the symbol 'green, 'yellow or 'red."
	     (draw-shape
	      (make-solid-disk :center (make-point-2d :x x-bulbs
						      :y (cond ((eq light-colour +red+) y-red)
							       ((eq light-colour +yellow+) y-yellow)
							       ((eq light-colour +green+) y-green)))
			       :radius bulb-radius
			       :colour light-colour)))
	   
	   (switch (current-colour next-colour other-colour)
	     "Turns off the current colour, turns on the next colour"
	     (and (clear-bulb current-colour)
		  (clear-bulb other-colour)
		  (draw-bulb next-colour)))

	   (next (current-colour)
	     "Switches a light's current colour and returns to the next colour."
	     (cond ((eq current-colour +red+)
		    (switch +red+ +green+ +yellow+)
		    +green+)
		   ((eq current-colour +yellow+)
		    (switch +yellow+ +red+ +green+)
		    +red+)
		   ((eq current-colour +green+)
		    (switch +green+ +yellow+ +red+)
		    +yellow+))))
    (background +black+)

    (setf current-colour (next current-colour))
    (sleep 0.5)))
