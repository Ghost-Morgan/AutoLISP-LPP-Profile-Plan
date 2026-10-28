;;; LPP_LocatePlanPoint.lsp   (command: LPP  /  LOCATEPLANPOINT)
;;; Civil 3D 2022 - plain AutoLISP driving the Civil 3D COM API.
;;; REVERSE of LAP (LAP_LocateAlignmentPoint.lsp).
;;;
;;; Workflow v1.2.2; field verification required.
;;;   1. Select the alignment (in plan), the profile view, OR the profile
;;;      (ground line / any profile line) drawn in the profile view.
;;;      Preselect (PICKFIRST) works for all three.
;;;        - alignment picked    -> its profile view is found automatically
;;;                                 (Alignment.ProfileViews, same as LAP v2.3);
;;;                                 if several exist you pick one.
;;;        - profile view picked -> the owning alignment is found by searching
;;;                                 every alignment's ProfileViews for that view.
;;;        - profile picked      -> the owning alignment is found by searching
;;;                                 every alignment's Profiles for that profile;
;;;                                 the view is the one whose bounding box holds
;;;                                 your pick point (else you pick one).
;;;   2. Pick any point in the profile view (loops until Enter).
;;;      Only the X position (= station) is used; elevation is ignored.
;;;   3. Station = ProfileView.FindStationAndElevationAtXY.
;;;   4. A perpendicular XLINE is made in plan through the alignment point at
;;;      that station. Direction comes from PointLocation at offsets -d/+d
;;;      (d = 1.0, falling back to 0.1, 0.01), so it is exactly perpendicular on
;;;      tangents, curves and spirals with no bearing maths.
;;;
;;; Layer/colour = same as LAP/XXL (XL-LINES, ACI 8). Made with ENTMAKEX, so the
;;; current layer/colour are never changed.
;;; NOTE: Civil 3D methods must be called with vlax-invoke-method.
;;; Use LAPDIAG from the LAP command repository to inspect a failing profile view.

(vl-load-com)

;;; ---- CONFIGURE ----
(setq *lpp:debug*    T)
(setq *lpp:layer*    "XL-LINES")
(setq *lpp:color*    8)
;;; -------------------

(defun lpp:say (s)
  (if *lpp:debug* (princ (strcat "\n[LPP] " s)))
  (princ)
)

(defun lpp:num (v)
  (if (= (type v) 'VARIANT) (vlax-variant-value v) v)
)

(defun lpp:prop (obj name / r)
  (if (vlax-property-available-p obj name)
    (progn
      (setq r (vl-catch-all-apply 'vlax-get-property (list obj name)))
      (if (vl-catch-all-error-p r) nil (lpp:num r))
    )
  )
)

(defun lpp:dxf-name (ename)
  (cdr (assoc 0 (entget ename)))
)

(defun lpp:view-name-p (dxf)
  (and dxf (wcmatch dxf "*PROFILE*VIEW*,*GRAPH*PROFILE*"))
)

(defun lpp:chainage (s / k txt)
  ;; 1234.5 -> "1+234.500"
  (setq k   (fix (/ s 1000.0))
        txt (rtos (- s (* 1000.0 k)) 2 3))
  (while (< (vl-string-position 46 txt) 3)
    (setq txt (strcat "0" txt)))
  (strcat (itoa k) "+" txt)
)

;;; ------------------------------------------------------------------
;;; selection: alignment OR profile view
;;; ------------------------------------------------------------------
(defun lpp:get-start (/ ss i en sel found pt dxf)
  ;; returns (ename pickpoint-or-nil)
  (setq ss (ssget "_I"))
  (if ss
    (repeat (setq i (sslength ss))
      (setq i   (1- i)
            en  (ssname ss i)
            dxf (lpp:dxf-name en))
      (if (or (= dxf "AECC_ALIGNMENT")
              (= dxf "AECC_PROFILE")
              (lpp:view-name-p dxf))
        (setq found en))
    )
  )
  (if found
    (sssetfirst nil nil)
    (if (setq sel (entsel "\nSelect the alignment (plan), the profile view, or the profile (ground line): "))
      (setq found (car sel)
            pt    (cadr sel)))
  )
  (if found (list found pt))
)

(defun lpp:profile-views (al quiet / coll out r pv)
  (setq coll (vl-catch-all-apply 'vlax-get-property (list al 'ProfileViews)))
  (cond
    ((vl-catch-all-error-p coll)
     (if (not quiet)
       (lpp:say (strcat "ProfileViews not readable: " (vl-catch-all-error-message coll))))
     nil)
    (T
     (setq r (vl-catch-all-apply
               (function (lambda () (vlax-for pv coll (setq out (cons pv out)))))
               nil))
     (if (and (vl-catch-all-error-p r) (not quiet))
       (lpp:say (strcat "Could not enumerate ProfileViews: " (vl-catch-all-error-message r))))
     (if (not quiet)
       (lpp:say (strcat (itoa (length out)) " profile view(s) on this alignment")))
     (reverse out))
  )
)

(defun lpp:choose-view (views / sel pv)
  (cond
    ((null views)
     (princ "\nThis alignment has no profile views.")
     nil)
    ((= (length views) 1) (car views))
    (T
     (setq sel (entsel "\nSeveral profile views - select the one to use: "))
     (if sel
       (progn
         (setq pv (vl-catch-all-apply 'vlax-ename->vla-object (list (car sel))))
         (if (and (not (vl-catch-all-error-p pv))
                  (member (vla-get-Handle pv) (mapcar 'vla-get-Handle views)))
           pv
           (progn (princ "\nThat is not one of this alignment's profile views.") nil)))
     )
    )
  )
)

(defun lpp:alignment-of-view (pv / h ss i obj found)
  ;; find the alignment whose ProfileViews contains this view
  (setq h  (vla-get-Handle pv)
        ss (ssget "_X" '((0 . "AECC_ALIGNMENT"))))
  (if ss
    (repeat (setq i (sslength ss))
      (setq i (1- i))
      (if (null found)
        (progn
          (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list (ssname ss i))))
          (if (and (not (vl-catch-all-error-p obj))
                   (member h (mapcar 'vla-get-Handle (lpp:profile-views obj T))))
            (setq found obj))
        )
      )
    )
  )
  (if found
    (lpp:say (strcat "owning alignment: " (vl-princ-to-string (lpp:prop found 'Name))))
    (princ "\nCould not find the alignment that owns this profile view. Run LPP again and select the alignment instead."))
  found
)

(defun lpp:alignment-of-profile (prof / h ss i obj coll p found)
  ;; find the alignment whose Profiles collection contains this profile
  (setq h  (lpp:prop prof 'Handle)
        ss (ssget "_X" '((0 . "AECC_ALIGNMENT"))))
  (cond
    ((null h) (princ "\nCould not read the profile's handle."))
    ((null ss) (princ "\nNo alignments found in the drawing."))
    (T
     (repeat (setq i (sslength ss))
       (setq i (1- i))
       (if (null found)
         (progn
           (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list (ssname ss i))))
           (if (not (vl-catch-all-error-p obj))
             (progn
               (setq coll (vl-catch-all-apply 'vlax-get-property (list obj 'Profiles)))
               (if (not (vl-catch-all-error-p coll))
                 (vl-catch-all-apply
                   (function
                     (lambda ()
                       (vlax-for p coll
                         (if (= (lpp:prop p 'Handle) h) (setq found obj)))))
                   nil)
               )
             )
           )
         )
       )
     )
    )
  )
  (if found
    (lpp:say (strcat "owning alignment: " (vl-princ-to-string (lpp:prop found 'Name))))
    (princ "\nCould not find the alignment that owns this profile. Run LPP again and select the alignment instead."))
  found
)

(defun lpp:view-at-point (views pt / v r lo hi a b hits)
  ;; the one profile view whose bounding box contains pt (nil if none/ambiguous)
  (if pt
    (progn
      (setq pt (trans pt 1 0))
      (foreach v views
        (setq lo nil
              hi nil
              r  (vl-catch-all-apply 'vla-getboundingbox (list v 'lo 'hi)))
        (if (and (not (vl-catch-all-error-p r)) lo hi)
          (progn
            (setq a (vlax-safearray->list (if (= (type lo) 'VARIANT) (vlax-variant-value lo) lo))
                  b (vlax-safearray->list (if (= (type hi) 'VARIANT) (vlax-variant-value hi) hi)))
            (if (and (<= (car a) (car pt) (car b))
                     (<= (cadr a) (cadr pt) (cadr b)))
              (setq hits (cons v hits)))
          )
        )
      )
    )
  )
  (if (= (length hits) 1) (car hits))
)

;;; ------------------------------------------------------------------
;;; profile view XY -> station,  station/offset -> plan XY
;;; ------------------------------------------------------------------
(defun lpp:station-at (pv pt / st el r)
  (setq r (vl-catch-all-apply
            (function
              (lambda ()
                (vlax-invoke-method pv 'FindStationAndElevationAtXY
                                    (float (car pt)) (float (cadr pt)) 'st 'el)))
            nil))
  (cond
    ((vl-catch-all-error-p r)
     (princ (strcat "\nFindStationAndElevationAtXY failed: " (vl-catch-all-error-message r)))
     nil)
    ((eq r :vlax-false)
     (princ "\nPoint is outside the profile view.")
     nil)
    ((null st)
     (princ "\nFindStationAndElevationAtXY returned no station.")
     nil)
    (T (lpp:num st))
  )
)

(defun lpp:point-at (al station offset / x y r)
  (setq r (vl-catch-all-apply
            (function
              (lambda ()
                (vlax-invoke-method al 'PointLocation
                                    (float station) (float offset) 'x 'y)))
            nil))
  (cond
    ((vl-catch-all-error-p r)
     (lpp:say (strcat "PointLocation (offset " (rtos offset 2 2) ") failed: "
                      (vl-catch-all-error-message r)))
     nil)
    ((or (null x) (null y)) nil)
    (T (list (lpp:num x) (lpp:num y) 0.0))
  )
)

(defun lpp:perp-dir (al station / d pa pb v len)
  ;; unit vector from the -d offset point to the +d offset point = perpendicular
  (foreach d '(1.0 0.1 0.01)
    (if (null v)
      (if (and (setq pa (lpp:point-at al station (- d)))
               (setq pb (lpp:point-at al station d)))
        (progn
          (setq v   (list (- (car pb) (car pa)) (- (cadr pb) (cadr pa)))
                len (sqrt (+ (* (car v) (car v)) (* (cadr v) (cadr v)))))
          (if (> len 1e-9)
            (setq v (list (/ (car v) len) (/ (cadr v) len) 0.0))
            (setq v nil))
        )
      )
    )
  )
  v
)

(defun lpp:make-xline (pt dir / lay)
  (setq lay *lpp:layer*)
  (if (not (tblsearch "LAYER" lay))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") (cons 2 lay) '(70 . 0)
                   (cons 62 *lpp:color*) '(6 . "Continuous")))
  )
  (entmakex (list '(0 . "XLINE") '(100 . "AcDbEntity")
                  (cons 8 lay) (cons 62 *lpp:color*)
                  '(100 . "AcDbXline")
                  (list 10 (car pt) (cadr pt) (caddr pt))
                  (list 11 (car dir) (cadr dir) 0.0)))
)

;;; ------------------------------------------------------------------
;;; main command
;;; ------------------------------------------------------------------
(defun c:LOCATEPLANPOINT (/ *error* olderr sel selpt dxf obj al pv views pick station smin smax p0 dir n)
  (setq olderr *error*)
  (defun *error* (msg)
    (if (and msg (not (member msg '("Function cancelled" "quit / exit abort"))))
      (princ (strcat "\nError: " msg)))
    (setq *error* olderr)
    (princ)
  )

  (cond
    ((null (setq sel (lpp:get-start)))
     (princ "\nNothing selected; nothing drawn."))
    ((progn
       (setq selpt (cadr sel)
             sel   (car sel)
             dxf   (lpp:dxf-name sel))
       (not (or (= dxf "AECC_ALIGNMENT") (= dxf "AECC_PROFILE") (lpp:view-name-p dxf))))
     (princ (strcat "\nSelected object is " (if dxf dxf "unknown")
                    " - select an alignment, a profile view or a profile.")))
    ((vl-catch-all-error-p
       (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list sel))))
     (princ "\nCould not get a COM object for that selection."))
    (T
     (cond
       ((= dxf "AECC_ALIGNMENT")
        (setq al obj
              pv (lpp:choose-view (lpp:profile-views obj nil))))
       ((= dxf "AECC_PROFILE")
        (setq al (lpp:alignment-of-profile obj))
        (if al
          (progn
            (setq views (lpp:profile-views al nil))
            (setq pv (cond ((lpp:view-at-point views selpt))
                           ((lpp:choose-view views)))))))
       (T
        (setq pv obj
              al (lpp:alignment-of-view obj)))
     )
     (cond
       ((or (null al) (null pv))
        (princ "\nAlignment / profile view not resolved; nothing drawn."))
       (T
        (setq smin (lpp:prop al 'StartingStation)
              smax (lpp:prop al 'EndingStation)
              n    0)
        (while (setq pick (getpoint "\nPick point in the profile view <finish>: "))
          (setq pick (trans pick 1 0))
          (if (setq station (lpp:station-at pv pick))
            (cond
              ((or (and (numberp smin) (< station (- smin 1e-6)))
                   (and (numberp smax) (> station (+ smax 1e-6))))
               (princ (strcat "\nStation " (lpp:chainage station) " is outside the alignment.")))
              ((null (setq p0 (lpp:point-at al station 0.0)))
               (princ "\nCould not get the alignment point at that station."))
              ((null (setq dir (lpp:perp-dir al station)))
               (princ "\nCould not work out the perpendicular direction at that station."))
              ((lpp:make-xline p0 dir)
               (setq n (1+ n))
               (princ (strcat "\nPerpendicular XLINE made at station " (lpp:chainage station)
                              " (" (rtos station 2 3) ").")))
              (T (princ "\nXLINE creation failed (layer locked/frozen?)."))
            )
          )
        )
        (princ (strcat "\n" (itoa n) " XLINE(s) made."))
       )
     )
    )
  )
  (setq *error* olderr)
  (princ)
)

(defun c:LPP () (c:LOCATEPLANPOINT))

(princ "\nLocatePlanPoint v1.2.2 loaded: LPP / LOCATEPLANPOINT (select alignment, profile view or profile; perpendicular XLINE; untested - run LAPDIAG on the profile view if a step fails).")
(princ)
