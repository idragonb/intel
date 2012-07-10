(defun c:recordxref ()
   (Setq xreffile (dcl_Control_GetCaption Intel-Standards-Checker_Form1_Label32))
   (Setq pathname (dcl_Control_GetCaption Intel-Standards-Checker_Form1_Label5))
   (setq x-file (strcat pathname "\\" xreffile))
   (setq x_file (open x-file "a"))
   (setq x-line (getxrefline))
   (write-line x-line x_file)
   (close x_file)
)

(defun getxrefline (/ retval)
   (setq dwg-name (getvar "dwgname"))
   (setq retval dwg-name)
   (setq first T)
   (while (setq tbllist (tblnext "BLOCK" first))
      (setq first nil)
      (setq c1 (cdr (assoc 1 tbllist)))
      (setq c2 (cdr (assoc 2 tbllist)))
      (if (not (hasline c2))
         (progn
            (if c1 ; is xref
               (progn
                  (setq xrefname c2)
                  (setq xrefpath c1)
                  (setq isloaded (chkifloaded tbllist))
                  (if isloaded (setq xrefloadstatus "Loaded")(setq xrefloadstatus "Not loaded"))
                  (setq filt (list (cons 2 c2)))
                  (setq ssset (ssget "X" filt))
                  (if ssset
                     (progn
                        (setq ent (ssname ssset 0))
                        (setq entlist (entget ent))
                        (setq c8 (cdr (assoc 8 entlist)))
                        (setq xreflayer c8)
                        (setq c10 (cdr (assoc 10 entlist)))
                        (setq xrefinsert (strcat
                                            (rtos (nth 0 c10) 2 2) ","
                                            (rtos (nth 1 c10) 2 2) ","
                                            (rtos (nth 2 c10) 2 2)
                                         ))
                     )
                  )
                  (setq retval (strcat retval "^" xrefname "|" xrefpath "|" xrefloadstatus "|" xreflayer "|" xrefinsert))
               )
            )
            
         )
      )
   )
   retval
)

(defun hasline (str / retval)
   (setq slen (strlen str))
   (setq nn 0)
   (setq retval nil)
   (while (< nn slen)
      (setq nn (1+ nn))
      (setq curlet (substr str nn 1))
      (if (= curlet "|")
         (setq retval T)
      )
   )
   retval
)


(defun chkifloaded (tbllist / retval)
   (setq c70 (cdr (assoc 70 tbllist)))
   (if (= c70 12)
      (setq retval nil)
      (setq retval T)
   )
   retval
)