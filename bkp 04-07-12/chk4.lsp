(load "excel utilities.lsp")

(defun chk4 (/ retval)
  (setq	dwgname
	 (vl-registry-read
	   "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker"
	   "Current drawing"
	 )
  )
  (putinexcel "thisdwg" dwgname)
    
  
  (setq result (getresult "chk4"))
  (if (= result "x")
    (copytab "chk4results" "chk4")
  )
)

(defun checkforgrid (thisdwg / retval)
   (setq retval "ok")
   (setq let78 (substr thisdwg 7 2))
   (setq gridname (strcat "G" (substr thisdwg 2 1) "G-0-" (substr thisdwg 7 2)))
   (if (not (tblsearch "BLOCK" gridname))
      (progn      
         (setq retval "X")
         (appendreport "-Grid not found ")
      )
   )
   retval
)

(defun chk4K (thisdwg / retval)
   (setq retval "ok")
   (if (islayout thisdwg)
      (setq retval (checkforgrid thisdwg))
   )
   (setq let3 (substr thisdwg 3 1))
   (if (and (/= let3 "A") (/= let3 "S"))
      (progn
         (setq archname (strcat "G" (substr thisdwg 2 1) "A" (substr thisdwg 4 3) "OV"))
         (setq structname (strcat "G" (substr thisdwg 2 1) "S" (substr thisdwg 4 3) "OV"))
         (if (not (or (tblsearch "BLOCK" archname) (tblsearch "BLOCK" structname)))
            (progn
               (setq retval "X")
               (appendreport "-No structural or Architectural background found ")
            )
         )
      )
   )
   retval
)

