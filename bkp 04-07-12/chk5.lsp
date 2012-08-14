
(load "excel utilities.lsp")

(defun chk5 (/ retval)
  (setq	dwgname
	 (vl-registry-read
	   "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker"
	   "Current drawing"
	 )
  )
  (putinexcel "thisdwg" dwgname)
  (setq result (getresult "chk5"))
  (if (= result "x")
    (copytab "chk5results" "chk5")
  )
)