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
- A curved member is an `arc` object (`:center`, `:radius`,
  `:start-angle`, `:end-angle`, radians counterclockwise from its
  `:right`), hidden when it is there only to build on, and everything on
  the curve is read from it: `(the curve (point-on-arc ANGLE))` is the
  point ANGLE along it from its start, `(the curve (tangent POINT))` the
  direction it runs at POINT (a vector one radius long, which
  `alignment` takes as it is), `(the curve start-to-end-angle)` the angle
  it spans and `(the curve total-length)` its length.
- A row of pieces along a curve -- the boxes of a bent strap, the links
  of a chain -- is a child `:sequence` whose members take `:center` from
  `point-on-arc` and `:orientation` from `tangent`, one hidden arc for
  each row, as the guard does below; rows that nest are arcs with the
  same center and different radii.  A point on a curve written as
  `(* r (sin a))` and `(* r (cos a))`, in a loop or anywhere else, is
  the arc worked out by hand: build the arc.
- An arc's angles are measured, not solved.  Find its center as a point
  (from the crown, one radius toward the chord); its half angle is then
  `(angle-between-vectors (subtract-vectors crown center)
  (subtract-vectors end center))`.  `asin`, `acos` and `atan` belong only
  in the numeric `defun` below.
- A part that must be measured before it can be placed -- its span
  decides where its far end goes -- is measured on a second, hidden copy
  of it left at the model's center.
- A table over a STATE input's range is read from a hidden sequence of
  whole models, one per value, never made with `set-slot!`.
- A value with no closed form is solved numerically in a `defun` whose
  doc string says so.  That is the one place for trigonometry.
- A list of values is made with `mapcar` over a list; for an index,
  `(mapcar #'(lambda (n) ...) (list-of-numbers 0 (1- count)))`.  No
  `loop`, and no `push` with `nreverse`.
- A slot cannot be defined from itself.  Inside a part's own definition
  `(the center)` IS the slot `center`, so a computed slot `(center
  (translate (the center) :up 10))` asks for itself without end, and the
  same goes for an `orientation` built from the part's own
  `face-normal-vector`.  A part does not compute its own `center` or
  `orientation`: its parent gives them, in the child's entry under
  `:objects`, where `(the center)` is the parent's.
- `leaves`, `children`, `index` and `parent` are every object's own
  messages: name no slot or child so.

### A worked example: a bar on a swinging link, with a bent guard

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
    (mapsend (list-elements (the samples)) :link-angle))

   ;; The guard's four pieces share its curve: each has a quarter of the
   ;; angle it spans and a quarter of its length.
   (guard-step (/ (the guard-curve start-to-end-angle) 4))
   (guard-piece-length (/ (the guard-curve total-length) 4)))

  :hidden-objects
  (;; Everywhere the free end can be, as far as the bar is concerned.
   (reach :type 'sphere
          :center (the fixed-pin)
          :radius (the bar-length))

   ;; The guard's center line: an arc about the frame pin, a clearance
   ;; outside the link's swing, a quarter turn from level on the side
   ;; away from the bar round to straight down.  An arc lies in its own
   ;; right/rear plane, so its :top is the axis the link swings about,
   ;; and its angles run from its :right.
   (guard-curve :type 'arc
                :center (the frame-pin)
                :radius (+ (the link-length) 40)
                :orientation (alignment :top (the (face-normal-vector :rear))
                                        :right (the (face-normal-vector :right)))
                :start-angle 0
                :end-angle (/ pi 2))

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

   ;; Each piece sits on the guard's curve at the middle of its own
   ;; quarter and lies along the curve there.
   (guard :type 'box
          :sequence (:size 4)
          :center (the guard-curve (point-on-arc (* (+ (the-child index) 1/2)
                                                    (the guard-step))))
          :orientation (alignment :right (the guard-curve (tangent (the-child center)))
                                  :rear (the (face-normal-vector :rear)))
          :width (the guard-piece-length)
          :length 40
          :height 6
          :display-controls (list :color :goldenrod))

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
