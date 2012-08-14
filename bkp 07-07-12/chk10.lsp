(defun chk10 (/ retval)
   (setq retval "ok")
   (setq let1 (substr (getvar "dwgname") 1 1))
   (if (= let1 "G")
      (setq retval (checkpsempty))
   )
   (if (= let1 "K")
      (setq retval (checkmsonlyxrefs))
   )
   retval
)


(defun checkmsonlyxrefs (/ retval thisretval)
   (setq ent (entnext))
   (setq retval "ok")
   (while (setq ent (entnext ent))
      (setq entlist (entget ent))
      (setq c0 (cdr (assoc 0 entlist)))
      (setq c67 (cdr (assoc 67 entlist)))
      (if (and (/= c0 "INSERT") (= c67 0))
         (setq thisretval "X")
         (if (= c67 0)
            (setq thisretval (checkifxref ent))
         )
      )
      (if (= thisretval "X")
         (setq retval "X")
      )
   )
   (IF (= (GETVAR "TILEMODE") 1)
      (PROGN
         (SETQ RETVAL "X")
         (appendreport "-Not in layout tab. ")
      )
   )
   retval
)

(defun checkifxref (ent / retval)
   (setq retval "ok")
   (setq entlist (entget ent))
   (setq c2 (cdr (assoc 2 entlist)))
   (setq intbl (tblsearch "BLOCK" c2))
   (setq c0 (cdr (assoc 0 entlist)))
   (setq c1 nil)
   (if intbl
      (setq c1 (cdr (assoc 1 intbl)))
      (appendreport "-Objects exist in ms other than xrefs. ")
   )
   (if c1
      (setq retval "ok")
   )
   retval
)   
(defun checkpsempty (/ retval)
   (setq ent (entnext))
   (setq nn 0)
   (setq retval "ok")
   (while (setq ent (entnext ent))
      (setq entlist (entget ent))
      (setq c67 (cdr (assoc 67 entlist)))
      (if (= c67 1)
         (setq nn (1+ nn))
      )
   )
   (if (> nn 1)
      (progn
         (setq retval "X")
         (appendreport "-Paperspace has " (itoa nn) " entities. ")
      )
   )
   retval
)