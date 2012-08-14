(defun chk16 (/ retval thisretval)
   (setq retval "ok")
   (setq thisretval "ok")
   (setq first T)
   (while (setq tbllist (tblnext "DIMSTYLE" first))
      (setq first nil)
      (setq c2 (cdr (assoc 2 tbllist)))
      (setq thisretval (checkdimstylename c2))
      (if (= thisretval "ok")
         (setq thisretval (checkstylevars c2))
      )
      (if (= thisretval "X")
         (setq retval "X")
      )
   )
   retval
)

(defun checkstylevars (thisdimname / retval)
   (setq retval "ok")
   (setq tbllist (tblsearch "DIMSTYLE" thisdimname))
   (setq c2 (cdr (assoc 2 tbllist))) ; STANDARD
   (if (not (hasline c2))
      (progn
         (setq c41 (cdr (assoc 41 tbllist))) ; 2.38
         (setq c40 (cdr (assoc 40 tbllist))) ; scalefactor
         (if (> (strlen thisdimname) (strlen "STANDARD"))
            (setq myscale (atoi (substr thisdimname 10)))
            (setq myscale 1)
         )
         (if (and
                (= (strcase (substr c2 1 8)) "STANDARD")
                (= c41 2.38)
                (= myscale c40)
             )
             (setq retval "ok")
             (setq retval "X")
         )
      )
   )
   retval
)

(defun checkdimstylename (thisdimname / retval)
   (setq retval "ok")
   (if (not (or (wcmatch (strcase thisdimname) "STANDARD") (wcmatch thisdimname "STANDARD_*") (wcmatch thisdimname "STANDARD-*")))
      (progn
         (if (not (hasline thisdimname))
            (progn
               (appendreport (strcat "-Dimstyle name: " thisdimname " not according to standard. "))
               (setq retval "X")
            )
         )
      )
   )
   retval
)
  