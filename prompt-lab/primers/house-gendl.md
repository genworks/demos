## Writing a Gendl model: the grammar, spelled out

You write Common Lisp with Gendl's `define-object` macro.  Read this
before your first write_model: it is the whole grammar you need, and
every example in it compiles as written.  Do not probe the macro
(macroexpand, keyword guessing); the forms below are the forms.

### 1. The shape of a define-object

```lisp
(define-object model (base-object)
  :input-slots
  ((width 600)
   (length 400)
   (height 25))

  :computed-slots
  ((top-color :steel-blue))

  :objects
  ((top :type 'box
        :width (the width) :length (the length) :height (the height)
        :center (the center)
        :display-controls (list :color (the top-color)))))
```

Each of these rules is a compiler error, or a model that silently
draws nothing, when it is broken:

- `(define-object NAME (MIXINS) KEYWORD VALUE KEYWORD VALUE ...)`.  The
  second form is the list of mixins: `(base-object)` for MODEL.  After
  it come keyword and value PAIRS at the top level -- `:input-slots`,
  `:computed-slots`, `:objects`, `:hidden-objects`, `:functions` -- each
  followed by its list.  Never wrap the pairs in one more list:
  `(define-object model (base-object) (:input-slots ...))` fails with
  "No such toplevel define-object keyword exists".
- No docstring after the name, and no `:documentation` string.
- A slot is `(name default)`.  The default is any expression.
- NAMES -- a recipe:
  - Every name in one define-object -- inputs, computed slots, children,
    functions -- is used ONCE ("duplicate slot name" otherwise).
  - A clash with Gendl shows as "The symbol listed below is a reserved
    word ..." with the name under it.  Rename and write again.
  - Refused outright, Gendl's own: `index`, `leaves`, `children`,
    `parent`, `half-width`, `half-length`, `half-height`.
  - Accepted but leave them alone -- they replace what the lab measures
    your model by: `bounding-box`, `volume`, `area`, `root`,
    `vertex-list`.
  - THE RULE BENDS for the inputs every object has: `width`, `length`
    and `height` (MODEL's envelope), `center`, `orientation` and
    `display-controls` are never reserved, and MODEL may define any of
    them either way, with the same effect:
    - as an INPUT, `(height 750)` under `:input-slots` -- the visitor
      gets a control for it in the viewer;
    - as a COMPUTED slot, `(height (* 2 (the width)))` under
      `:computed-slots` -- derived from the inputs, and no control.
    Make the envelope an input when a person should change it, computed
    when it follows from other inputs.
  - The safe pattern for your own names: a specific noun, often two
    words -- `leg-height`, `top-center`, `spoke-count`, `bolt-circle`.
- A function's arguments are distinct names: `(plate-area (w l) ...)`,
  never `(plate-area (d d) ...)`.
- Read a slot, inside define-object, with `(the name)` -- an input, a
  computed slot, `center`, anything.  A bare `width` is an unbound
  variable.  A child's slot: `(the top width)`; a member of a sequence:
  `(the legs 0)`.
- In a child's inputs `(the ...)` is still the object being defined
  (the parent).  The child itself, for instance its place in a
  sequence, is `(the-child index)`.
- A slot cannot be defined from itself.  `(center (translate (the
  center) :up 10))` under a helper object's `:computed-slots` asks for
  `center` without end and fails with "Stack overflow"; so does
  `(depth (the depth))`.  A helper object does not compute its own
  `center` or `orientation`: give them where the helper is used, in the
  child's entry under `:objects`, where `(the center)` is the parent's.
- A child is `(name :type 'TYPE :input value :input value ...)`.  The
  type is a quoted symbol; its inputs are keyword and value pairs.
  Spell input names exactly: describe_object lists a type's inputs.
- `:functions` are `((name (args) body...))`, called as
  `(the (name arg))`.
- Arithmetic helpers: `(half x)`, `(twice x)`; `pi` is Common Lisp's.

### 2. Placing parts

- Every object has a `center` (a point) and an `orientation`.  A child
  given no `:center` sits at its parent's center, so a model whose parts
  all lack one is a pile at the origin.  Place each part:
  `:center (translate (the center) :right x :rear y :up z)`.
- Directions: `:right` / `:left` along X, `:rear` / `:front` along Y,
  `:up` / `:down` (or `:top` / `:bottom`) along Z.  Z is up.
- MODEL's own `width`, `length` and `height` (inputs every object has)
  are its ENVELOPE: a box centered on `(the center)`, which check_model's
  overall size counts.  Name the overall dimensions `width` (X),
  `length` (Y) and `height` (Z), and place every part INSIDE that box,
  from its center: the envelope's top face is `:up (half (the height))`,
  its bottom `:down (half (the height))`.  A part built up from the
  floor (`:up` from 0) sticks out above the envelope, and the size
  comes out half as tall again.  Dimensions with other names (`depth`,
  `span`) are not part of the envelope.
- THE RECIPE for any part: place it by its height above the floor.
  For a part of height `h` whose bottom is `z` above the floor:
  `:center (translate (the center) :down (half (the height)) :up (+ z (half h)))`.
  Sideways the same way from the left and front faces, or from the
  center line.  Parts placed this way can never overshoot the envelope.
- `box`: `:width` along X, `:length` along Y, `:height` along Z,
  centered on its `:center`.
- `cylinder`: `:radius`, `:length`, `:center`.  Its axis runs along its
  own Y (front to rear).  To turn the axis, give
  `:orientation (alignment :rear VECTOR)`: upright is
  `(alignment :rear (the (face-normal-vector :top)))`, along X is
  `(alignment :rear (the (face-normal-vector :right)))`.  `cone` is the
  same, with `:radius-1` and `:radius-2`.  `:start` and `:end` are not
  inputs.
- `sphere`: `:radius`, `:center`.
- An extruded outline: `global-polygon-projection` with `:vertex-list`
  (points, e.g. `(translate (the center) :right 10 :rear 5)`),
  `:projection-vector` and `:projection-depth`.
- On this free engine there are no booleans: a hole can be drawn as a
  cylinder but not cut.  Say so in the reply.

### 3. Repeated parts

`:sequence (:size N)` makes N members, indexed 0 to N-1 by
`(the-child index)`.  Use it for any repeat; never write `leg-0`,
`leg-1` ... by hand.  For a grid, one sequence of rows times columns,
with `(floor i columns)` and `(mod i columns)`.

A list of VALUES (the points of an outline, say) is made with `mapcar`
over a list; for an index,
`(mapcar #'(lambda (n) ...) (list-of-numbers 0 (1- count)))`.  Lean on
the do and map families (the iteration rule in your instructions names
the form for each use, at the same cost); `loop` (in the shapes the rule
lists) or iterate only where none of them fits well.

A mixin list names only what the object needs from it: `(base-object)`
for a part, `()` for an object that only holds values.  Never name
`vanilla-mixin`; every object has it already.

### 4. Inputs a person can change

Every input-slot of MODEL gets a control in the viewer.  Make an input
only what a person would change; derived values go in
`:computed-slots`.  Bounds and choices go in a computed slot named
`input-controls`:

```lisp
(input-controls (list :count (list :kind :integer :min 2 :max 12)))
```

### 5. How to work with the tools

1. Decide the envelope in mm (x y z).  Think briefly; the grammar above
   is settled.
2. write_model: the whole file -- helper definitions, then MODEL.  No
   `in-package`.
3. ALWAYS check_model with expected_size after every change that
   compiles.  Read the leaf count, the overall size and any pile warning.
4. Then view_model: it says, in words, where every part is, what touches
   and what OVERLAPS what, and which parts are LOOSE (joined to nothing).  Compare it with the
   request: legs under the top, a handle on the door, wheels touching the
   ground.
5. Fix what you see ONE ENTRY AT A TIME, by name: set_slot changes one
   input, computed slot, child or function (you give only what follows
   its name -- the tool writes the parentheses around it); add_slot adds
   one; remove_slot removes one; replace_form replaces a whole
   define-object or defun.  They cannot unbalance the file.
   A CHILD OBJECT is one entry of `:objects`, named for itself, so it
   is added, changed and removed by ITS name, one child per entry:

   ```
   add_slot     object "model"  section "objects"  slot "shelf"
                value  ":type 'box :width (the width) :length 300
                        :height 18 :center (translate (the center) :up 100)"
   set_slot     object "model"  slot "shelf"  value ":type 'box ... :height 25 ..."
   remove_slot  object "model"  slot "shelf"
   ```

   SLOT is never `objects` (that is the section), and VALUE starts at
   `:type`: no parentheses around the child, no list of children.
   Several entries -- inputs, children, functions, in any sections --
   go in ONE add_slot as ENTRIES, checked first and written together;
   one bad entry writes none, and the error names it:

   ```
   add_slot  object "model"
             entries [{section "input-slots" slot "width"  value "600"},
                      {section "objects"     slot "top"
                       value ":type 'box :width (the width) :length 400 :height 20"},
                      {section "objects"     slot "leg"
                       value ":type 'box :width 40 :length 40 :height 700"}]
   ```

   new_object_def takes ENTRIES the same way, so a helper object can be
   started and filled in one call.
6. When the numbers match and view_model shows the parts where they
   belong, stop and reply.  A simple model that is right beats an
   elaborate one that does not compile.

- evaluate takes ONE expression; wrap several in `(progn ...)`.  It is
  Common Lisp: `handler-case` and `ignore-errors`, not Emacs Lisp's
  `condition-case`.  To look at the model:
  `(the-object (make-object 'model) bounding-box)`.
- Colours: `:display-controls (list :color :steel-blue)` -- e.g.
  `:steel-blue`, `:slate-gray`, `:dim-gray`, `:sienna`,
  `:dark-olive-green`, `:firebrick`, `:goldenrod`.

### 6. When check_model or the compiler complains

- "Unexpected end of file" or "Reader error": the parentheses do not
  balance, and the error says which form is left open.  Write that
  define-object again with replace_form (or the file with write_model),
  closing every form as you go -- each child form `(name :type 'box ...)`
  closed on its own last line, then `)` for `:objects`, then `)` for the
  define-object.  After that, change it with set_slot.
- SIZE MISMATCH where a size is too big by about half the expected
  height (or width, or length): the parts were placed up from the center,
  not from the floor.  Use the recipe in section 2; do not change the
  inputs to make the numbers agree.
- SIZE MISMATCH where the parts are right and only your expected_size
  was a guess: say so in the reply and stop.  Two fixes for the same
  mismatch is the most to try.
- In evaluate there is no `self`: `(the width)` fails there.  Ask the
  built model: `(the-object (make-object 'model) width)`.
- When check_model says "Expected size matched" and lists the parts you
  meant, reply to the visitor and STOP.  Do not keep improving.

### 7. Worked examples

A table: a top and four legs from one sequence, inside the envelope.

```lisp
(define-object model (base-object)
  :input-slots
  ((width 1200) (length 700) (height 750) (top-thickness 30) (leg-size 60))

  :computed-slots
  ((leg-height (- (the height) (the top-thickness)))
   (leg-inset (half (the leg-size))))

  :objects
  ((top :type 'box
        :width (the width) :length (the length) :height (the top-thickness)
        :center (translate (the center) :up (- (half (the height)) (half (the top-thickness))))
        :display-controls (list :color :sienna))
   (legs :type 'box
         :sequence (:size 4)
         :width (the leg-size) :length (the leg-size) :height (the leg-height)
         :center (translate (the center)
                            :right (* (if (evenp (the-child index)) -1 1)
                                      (- (half (the width)) (the leg-inset)))
                            :rear (* (if (< (the-child index) 2) -1 1)
                                     (- (half (the length)) (the leg-inset)))
                            :down (half (the top-thickness)))
         :display-controls (list :color :dim-gray))))
```

A shaft along X with a flange at one end: cylinders turned by
orientation.

```lisp
(define-object model (base-object)
  :input-slots
  ((shaft-length 300) (shaft-diameter 30) (flange-diameter 90) (flange-thickness 12))

  :computed-slots
  ((along-x (the (face-normal-vector :right))))

  :objects
  ((shaft :type 'cylinder
          :radius (half (the shaft-diameter)) :length (the shaft-length)
          :center (the center)
          :orientation (alignment :rear (the along-x))
          :display-controls (list :color :steel-blue))
   (flange :type 'cylinder
           :radius (half (the flange-diameter)) :length (the flange-thickness)
           :center (translate (the center)
                              :left (- (half (the shaft-length)) (half (the flange-thickness))))
           :orientation (alignment :rear (the along-x))
           :display-controls (list :color :slate-gray))))
```

A bookshelf: two sides and a changeable number of shelves, inside the envelope.

```lisp
(define-object model (base-object)
  :input-slots
  ((width 800) (length 300) (height 1800) (board 18) (shelf-count 5))

  :computed-slots
  ((input-controls (list :shelf-count (list :kind :integer :min 2 :max 10)))
   (inner-width (- (the width) (twice (the board))))
   (shelf-pitch (/ (- (the height) (the board)) (1- (the shelf-count)))))

  :objects
  ((sides :type 'box
          :sequence (:size 2)
          :width (the board) :length (the length) :height (the height)
          :center (translate (the center)
                             :right (* (if (zerop (the-child index)) -1 1)
                                       (- (half (the width)) (half (the board)))))
          :display-controls (list :color :sienna))
   (shelves :type 'box
            :sequence (:size (the shelf-count))
            :width (the inner-width) :length (the length) :height (the board)
            ;; the bottom shelf on the envelope's floor, the rest above it
            :center (translate (the center)
                               :down (half (the height))
                               :up (+ (half (the board)) (* (the-child index) (the shelf-pitch))))
            :display-controls (list :color :goldenrod))))
```

A disk with a ring of bolts: polar placement from the index, and a
helper object of its own.

```lisp
(define-object bolt (base-object)
  :input-slots
  ((diameter 12) (shank-length 40))

  :objects
  ((shank :type 'cylinder
          :radius (half (the diameter)) :length (the shank-length)
          :center (the center)
          :orientation (alignment :rear (the (face-normal-vector :top)))
          :display-controls (list :color :dim-gray))))

(define-object model (base-object)
  :input-slots
  ((disk-diameter 200) (disk-thickness 15) (bolt-count 8) (bolt-circle 160))

  :computed-slots
  ((input-controls (list :bolt-count (list :kind :integer :min 3 :max 24))))

  :objects
  ((disk :type 'cylinder
         :radius (half (the disk-diameter)) :length (the disk-thickness)
         :center (translate (the center) :up (half (the disk-thickness)))
         :orientation (alignment :rear (the (face-normal-vector :top)))
         :display-controls (list :color :slate-gray))
   (bolts :type 'bolt
          :sequence (:size (the bolt-count))
          :shank-length 40
          :center (let ((angle (* 2 pi (/ (the-child index) (the bolt-count)))))
                    (translate (the center)
                               :right (* (half (the bolt-circle)) (cos angle))
                               :rear (* (half (the bolt-circle)) (sin angle))
                               :up 20)))))
```
