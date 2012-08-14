(defun chk9 (/ retval)
   (setq retval "ok")
   (setq retval (checklayertbl))
   retval
)

(defun checklayertbl ( / retval)
   (setq first T)
   (setq retval "ok")
   (while (setq tbllist (tblnext "LAYER" first))
      (setq first nil)
      (setq c2 (cdr (assoc 2 tbllist)))
      (setq isgood (chkgoodlayer c2))
      (if (not isgood)
         (progn
            (setq retval "X")
            (if (not (hasline c2))
               (appendreport (strcat "-Layer " c2 " is not in list. "))
            )
         )
      )
   )

   retval
)

(defun chkgoodlayer (mylayer / retval)
   (setq l_file (open (strcat installdirectory "good-layers.txt") "r"))
   (setq retval nil)
   (while (setq thislayer (read-line l_file))
      (if (or (= thislayer mylayer) (hasline mylayer))
         (setq retval T)
      )
   )
   (close l_file)
   retval
)
   
