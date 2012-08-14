(load "excel utilities.lsp")

(defun chk6 (/ retval)
  (setq	dwgname
	 (vl-registry-read
	   "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker"
	   "Current drawing"
	 )
  )
  (putinexcel "thisdwg" dwgname)
  (setq result (getresult "chk6"))
  (if (= result "x")
    (copytab "chk6results" "chk6")
  )
)

