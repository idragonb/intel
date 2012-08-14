(defun chk12 (/ retval)
   (setq retval "ok")
   (setq first T)
   (while (setq tbllist (tblnext "BLOCK" first))
      (setq first nil)
      (setq c2 (cdr (assoc 2 tbllist)))
      (if (not (hasline c2))
         (cond
            ((and (wcmatch c2 "*$*")(not (hasline c2))) (setq retval (reportbadblock c2)))
            ((and (< (strlen c2) 3)(not (hasline c2))) (setq retval (reportbadblock c2)))
         )
      )
   )
   retval
)
         
(defun reportbadblock (c2 / retval)
    (appendreport (strcat "-Block: " c2 " not according to standard. "))
   "X"
)
