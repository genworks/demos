
Hi Claude,

We're working on representing classical NACA profile curves as NURBS
curves in Genworks GDL


## Technical Setup

### MCP Tools in Use
- **`mcp__skewed-emacs__skewed-emacs__lisp_eval`** - For file editing and navigation
- **`mcp__genworks-gdl-non-smp__genworks-gdl-non-smp__lisp_eval`** - For Genworks GDL development and testing
- **`mcp__genworks-gdl-non-smp__genworks-gdl-non-smp__ping_lisp`** - Connection testing

 Skim the mcp api docs for each of the above before serious use! Use
 skewed-emacs lisp_eval for file spelunking and editing after careful
 review of its mcp documentations.


### Development Environment

In reference to the following, use skewed-emacs lisp_eval to access
the source files mentioned in `/projects/naca-nurbs/dev.lisp`, on an
as-needed basis.

```lisp
;;
;; Ensure the dev system is compiled/loaded
;;
gdl-user> (load "/projects/naca-nurbs/dev")
t
;; Create test instance  
gdl-user> (make-self 'naca-nurbs:naca-nurbs-curves 
                     :airfoil :23012 
                    :n-points 108)
#<naca-nurbs:naca-nurbs-curves 1>
;;
;; Some basic sanity checks:
;;
;; (note GDL's `(the ..)` expands to `(theo self ..)`
;;
gdl-user> (the approx-tolerance)
5.0e-4
gdl-user> (theo self approx-tolerance) ;; identical operation
5.0e-4 
gdl-user> (the n-points)
108
gdl-user> (mapcar #'length (plist-values (the sectioned-points)))
(17 92 13 96)
gdl-user> (the child-keys)
(:full-upper-fitted :full-lower-fitted :nose-upper-fitted
 :nose-lower-fitted :main-upper-fitted :main-lower-fitted
 :nose-upper-approx :nose-lower-approx :main-upper-approx
 :main-lower-approx :nose-upper-elev :nose-lower-elev :main-upper-elev
 :main-lower-elev)
gdl-user> (mapcan #'(lambda(key)(list key (length (the (evaluate key) control-points))))
		  (the child-keys))
(:full-upper-fitted 216 :full-lower-fitted 216 :nose-upper-fitted 34
 :nose-lower-fitted 26 :main-upper-fitted 184 :main-lower-fitted 192
 :nose-upper-approx 7 :nose-lower-approx 7 :main-upper-approx 13
 :main-lower-approx 13 :nose-upper-elev 11 :nose-lower-elev 11
 :main-upper-elev 21 :main-lower-elev 21)
```

Example of perturbing and probing the model:

```
gdl-user> (make-self 'naca-nurbs:naca-nurbs-curves)
; No value
gdl-user> (the approx-tolerance)
5.0e-4
gdl-user> (length (the nose-upper-elev control-points))
11
gdl-user> (the (set-slot! :approx-tolerance 0.001))
nil
gdl-user> (the approx-tolerance)
0.001
gdl-user> (length (the nose-upper-elev control-points))
11
gdl-user> (the (set-slot! :approx-tolerance 0.01))
nil
gdl-user> (length (the nose-upper-elev control-points))
6
gdl-user> (the (set-slot! :approx-tolerance 0.0001))
nil
gdl-user> (length (the nose-upper-elev control-points))
16
gdl-user> (the (set-slot! :approx-tolerance 0.001))
nil
gdl-user> (length (the nose-upper-elev control-points))
11
```

Some more examples of playing around:

```
gdl-user> self
#<naca-nurbs:naca-nurbs-curves 48>
gdl-user> (the full-upper-fitted total-length)
1.0324885029744688
gdl-user> (+ (the nose-upper-fitted total-length)
	     (the main-upper-fitted total-length))
1.0324887185081164
gdl-user> (+ (the nose-upper-approx total-length)
	     (the main-upper-approx total-length))
1.034590723861417
gdl-user> (the approx-tolerance)
0.005
gdl-user> (the (set-slot! :approx-tolerance 0.001))
nil
gdl-user> (the full-upper-fitted total-length)
1.0324885029744688
gdl-user> (+ (the nose-upper-approx total-length)
	     (the main-upper-approx total-length))
1.0325989959866453
gdl-user> (+ (the nose-upper-fitted total-length)
	     (the main-upper-fitted total-length))
1.0324887185081164

gdl-user> (the nose-lower-approx achieved-tolerance)
5.338167893730239e-4
gdl-user> (the quality-report)
[... returns nested plist ...]

```


## Reference docs for curve primitives

Read these documents before attempting to generate any code involving
the GDL `curve` or its subclasses (`fitted-curve`,
`approximated-curve`, etc):

These paths are accessible using http_request tool of
genworks-gdl-non-smp mcp server:


  /yadd
  /package-dokumentations/16/index.html
  /package-dokumentations/16/object-docs/dokumentation/25/index.html
  /package-dokumentations/16/object-docs/dokumentation/2/index.html
  /package-dokumentations/16/object-docs/dokumentation/37/index.html


## Source code for curve primitives

Refer to the following source directory if detailed implementation
info is needed for any curve primitives:

All the high-level Gendl object source codes are in
/projects/gendl/surf/source/


  
## Current Goals

### We imported a 0021 curve from SolidWorks

I've scaled, oriented, and translated it into canonical position and
sizing, as `(the imported-0021-lower)` and `(the
imported-0021-upper)`. Check a quality-report, which now contains
comparisons with the imported 0021.



## Workflow Lessons: Editing Lisp Files with skewed-emacs

### Key Learnings from 2024-09-21 Session

**Problem**: Initial attempts to search and edit Lisp files using skewed-emacs lisp_eval failed due to workflow issues.

**Root Issues Identified:**
1. **Buffer State Management**: Buffers can get killed or become invalid between lisp_eval calls
2. **Paredit Function Availability**: Not all paredit functions may be available (e.g., `paredit-kill-word`)
3. **Search Strategy**: Need to use exact string matching, not approximate searches

**Working Solution Pattern:**
```elisp
(progn
  ;; 1. Always re-open file fresh to ensure buffer state
  (find-file "/path/to/file.lisp")
  ;; 2. Enable paredit mode for Lisp files
  (when (fboundp 'paredit-mode)
    (paredit-mode 1))
  ;; 3. Use simple search-and-replace for text changes
  (goto-char (point-min))
  (when (search-forward "exact-old-text" nil t)
    (replace-match "new-text"))
  ;; 4. Always save after successful edit
  (save-buffer)
  "Success message")
```

**What Works:**
- `search-forward` with exact strings
- `replace-match` for simple replacements
- Fresh `find-file` calls to ensure buffer validity
- `save-buffer` to persist changes

**What Doesn't Work:**
- Assuming buffer state persists between lisp_eval calls
- Using non-existent paredit functions like `paredit-kill-word`
- Approximate string searches without exact matches
- Complex structural editing without verifying function availability

**Full Working Workflow:**

### For NACA NURBS Project Files
1. **Edit**: Use skewed-emacs lisp_eval with the working pattern above
2. **Compile**: `(load "/projects/naca-nurbs/dev.lisp")` via genworks-gdl-non-smp lisp_eval
3. **Test**: Check HTTP endpoint with genworks-gdl-non-smp http_request tool
4. **Verify**: Confirm changes appear in generated HTML

### For Core GDL System Files
When modifying files under `/projects/gendl/` (e.g., viewport components, renderers, etc.):
1. **Edit**: Use skewed-emacs lisp_eval with the working pattern above
2. **Update dev.lisp**: Add compilation/loading of the modified files to `/projects/gendl/dev.lisp`
3. **Compile GDL Changes**: `(load "/projects/gendl/dev.lisp")` via genworks-gdl-non-smp lisp_eval
4. **Test**: Check HTTP endpoint with genworks-gdl-non-smp http_request tool
5. **Verify**: Confirm changes appear in generated HTML

**Important Notes:**
- The `/projects/gendl/dev.lisp` file must be manually maintained to include compilation/loading of any modified GDL core files
- `(load "/projects/gendl/dev.lisp")` becomes the standard way to effectuate any changes made since the last Docker build
- Normally, changes under `/projects/gendl/` do **not** require reloading application-specific dev.lisp files (like `/projects/naca-nurbs/dev.lisp`)
- **Exception**: If any macros were redefined in the GDL core, then dependent code needs to be recompiled/loaded: `(load "/projects/naca-nurbs/dev.lisp")`

**Example Success Case:**
- Changed "SVG Visualization" to "2D Visualization" in ui.lisp
- System compiled successfully
- Change immediately visible in web interface
