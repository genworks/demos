## Geometry that follows from other geometry: construct it, intersect it, measure it

For a linkage, a pivot, a curved member, anything a STATE input moves.
Do not work such geometry out with sin, cos and atan in computed slots:
build the objects, intersect them and measure the result.  The model
then says what the mechanism is, and its construction is in the tree to
be inspected.

- Points are 3D points and directions are vectors, never (x z) pairs.
  Between points: `(midpoint a b)`, `(subtract-vectors b a)` (the
  direction from a to b), `(3d-distance a b)`.
- An angle is measured: `(angle-between-vectors-d v1 v2 axis t)` is the
  angle from v1 to v2 in degrees, signed by the right-hand rule about
  `axis`.
- A part gets a `:center` and an `:orientation` -- `(alignment :right
  VECTOR :rear VECTOR)` -- and places its own pieces with `translate`,
  which then moves along the part's own faces.  Place a piece from a
  sibling's face where it can be: `(translate (the seat (face-center
  :top)) :up 10)`.  Write no frame transform of your own.
- A part given by its two ends is a `c-cylinder`: `:start`, `:end`,
  `:radius`.
- A point held by two parts: each part publishes what it allows as a
  hidden object -- a `circle` (a pin on a swinging link), a `sphere` (a
  pin at a fixed distance from another) -- and the assembly intersects
  them with `inter-circle-sphere`, as below.  `(inter-line-plane point
  direction plane-point plane-normal)` is where a line meets a plane.
- A `circle` or an `arc` lies in its own right/rear plane: its `:top` is
  its axis.  Give it `:orientation (alignment :top AXIS)` unless its
  parent's `:top` is already that axis.
- A curved member is an `arc` (`:center`, `:radius`, `:start-angle`,
  `:end-angle`, radians counterclockwise from its `:right`).  Pieces sit on
  it at `(the curve (point-on-arc ANGLE))`, ANGLE from the arc's start,
  and turn with `(the curve (tangent POINT))`; its length is `(the curve
  total-length)`.
- A part that must be measured before it can be placed -- its span
  decides where its far end goes -- is measured on a second, hidden copy
  of it left at the model's center.
- A table over a STATE input's range is read from a hidden sequence of
  whole models, one per value, never made with `set-slot!`.
- A value with no closed form is solved numerically in a `defun` whose
  doc string says so.  That is the one place for trigonometry.
- `leaves`, `children`, `index` and `parent` are every object's own
  messages: name no slot or child so.

### A worked example: a bar on a swinging link

```lisp
(define-object model (base-object)

  :input-slots
  (("Length of the link between its pins, mm" link-length 120)
   ("STATE: length of the bar between its pins, mm" bar-length 560))

  :computed-slots
  ((input-controls (list :link-length (list :min 100 :max 150 :step 5)
                         :bar-length (list :min 530 :max 640 :step 5)))

   (fixed-pin (translate (the center) :left 300))
   (frame-pin (translate (the center) :right 300 :up 110))

   ;; The bar's free end is on the link's swing circle and one bar length
   ;; from the fixed pin.  NIL or T picks which of the two such points.
   (free-pin (let ((circle (the link swing-circle))
                   (sphere (the reach)))
               (inter-circle-sphere (the-object circle center)
                                    (the-object circle radius)
                                    (the-object circle (face-normal-vector :top))
                                    (the-object sphere center)
                                    (the-object sphere radius)
                                    nil)))

   ("Angle of the link from straight down, degrees (+ = swung to the right)"
    link-angle
    (angle-between-vectors-d (the (face-normal-vector :bottom))
                             (subtract-vectors (the free-pin) (the frame-pin))
                             (the (face-normal-vector :front))
                             t))

   ("List of link angles, degrees: one for each bar length in samples"
    link-angle-table
    (mapsend (list-elements (the samples)) :link-angle)))

  :hidden-objects
  (;; Everywhere the free end can be, as far as the bar is concerned.
   (reach :type 'sphere
          :center (the fixed-pin)
          :radius (the bar-length))

   ;; One whole model for each bar length.  Each computes only what is
   ;; asked of it.
   (samples :type 'model
            :sequence (:size 5)
            :bar-length (+ 530 (* 25 (the-child index)))
            :pass-down (link-length)))

  :objects
  ((bar :type 'c-cylinder
        :start (the fixed-pin)
        :end (the free-pin)
        :radius 12
        :display-controls (list :color :steel-blue))

   (link :type 'swing-link
         :center (the frame-pin)
         :free-end (the free-pin)
         :swing-axis (the (face-normal-vector :rear))
         :pass-down (link-length))

   (frame :type 'box
          :center (translate (the center) :up 200)
          :width 720
          :length 40
          :height 40
          :display-controls (list :color :dim-gray))))


(define-object swing-link (base-object)

  :input-slots
  (free-end swing-axis link-length)

  :hidden-objects
  (;; Everywhere the free end can be, as far as the link is concerned.  A
   ;; circle lies in its own right/rear plane, so its :top is the axis.
   (swing-circle :type 'circle
                 :radius (the link-length)
                 :orientation (alignment :top (the swing-axis))))

  :objects
  ((arm :type 'c-cylinder
        :start (the center)
        :end (the free-end)
        :radius 8
        :display-controls (list :color :firebrick))))
```
