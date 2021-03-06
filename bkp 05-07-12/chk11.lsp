(defun chk11 (/ retval thisretval)
   (checkfontstyles)
   (setq retval "ok")
   (setq ssstandard (ssget "X" '((7 . "Standard")(0 . "TEXT"))))
   (setq ssmono (ssget "X" '((7 . "MONO")(0 . "TEXT"))))
   (setq sstitle (ssget "X" '((7 . "TITLE")(0 . "TEXT"))))
   (setq sssubtitle (ssget "X" '((7 . "SUBTITLE")(0 . "TEXT"))))
   (appendreport "-Text scale: ")
   (if ssstandard
      (setq thisretval (checktexthtssame ssstandard 2.38))
   )
   (if (= thisretval "X")
      (setq retval "X")
   )

   (if ssmono
      (setq thisretval (checktexthtssame ssmono 2.38))
   )
   (if (= thisretval "X")
      (setq retval "X")
   )

   (if sstitle
      (setq thisretval (checktexthtssame sstitle 6.35))
   )
   (if (= thisretval "X")
      (setq retval "X")
   )

   (if sssubtitle
      (setq thisretval (checktexthtssame sssubtitle 4.76))
   )
   (if (= thisretval "X")
      (setq retval "X")
   )
   (if (= retval "X")
      (appendreport " not valid. ")
      (appendreport " all ok. ")
   )
   (checkfontstyles)
   retval
)

(defun checktexthtssame (ssset thisbaseht / nn retval)
   (setq retval "ok")
   (setq ssslen (sslength ssset))
   (setq nn 0)
   (setq c40 nil)
   (while (setq ent (ssname ssset nn))
      (setq nn (1+ nn))
      (setq entlist (entget ent))
      (setq c40 (cdr (assoc 40 entlist)))
      (setq thisscale  (/ c40 thisbaseht))
      (if (and (not (scaleok thisscale)) (/= retval "X"))
         (progn
            (appendreport (rtos thisscale))
            (appendreport ",")
            (setq retval "X")
         )
      )
   )

   retval
)

(defun scaleok (thisscale / retval)
   (setq retval nil)
   (cond
      ((equal thisscale 1 0.01) (setq retval T))
      ((equal thisscale 2 0.02) (setq retval T))
      ((equal thisscale 5 0.05) (setq retval T))
      ((equal thisscale 10 0.1) (setq retval T))
      ((equal thisscale 20 0.2) (setq retval T))
      ((equal thisscale 25 0.25) (setq retval T))
      ((equal thisscale 50 0.5) (setq retval T))
      ((equal thisscale 75 0.75) (setq retval T))
      ((equal thisscale 100 1.0) (setq retval T))
      ((equal thisscale 200 2.0) (setq retval T))
      ((equal thisscale 250 2.5) (setq retval T))
   )
   retval
)
(defun checkfontstyles (/ first tbllist c2 c3)
   (setq first T)
   (while (setq tbllist (tblnext "STYLE" first))
      (setq first nil)
      (setq c2 (cdr (assoc 2 tbllist)))
      (setq c3 (cdr (assoc 3 tbllist)))
      (if (and (= (strcase c2) "STANDARD") (/= c3 "ARIAL.TTF"))
         (appendreport (strcat "-Problem with style " c2 " change to ARIAL"))
      )
      (if (and (= (strcase c2) "SUBTITLE") (/= c3 "ARIALBD.TTF"))
         (appendreport (strcat "-Problem with style " c2 " change to ARIAL BOLD"))
      )
      (if (and (= (strcase c2) "TITLE") (/= c3 "ARIALBD.TTF"))
         (appendreport (strcat "-Problem with style " c2 " change to ARIAL BOLD"))
      )
      (if (and (= (strcase c2) "MONO") (/= (strcase c3) "MONOS.TTF"))
         (appendreport (strcat "-Problem with style " c2 " change to Monospac821 BT"))
      )
      (if (and (not (hasline c2)) (/= (strcase c2) "MONO")(/= (strcase c2) "TITLE")(/= (strcase c2) "SUBTITLE")(/= (strcase c2) "STANDARD") )
         (progn
            (appendreport (strcat "-Problem with style " c2 ": illegal style "))
            (setq retval "X")
         )
      )
   )
)

(defun hasline (str / retval)
   (wcmatch str "*|*")
)