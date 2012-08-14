(load "excel utilities.lsp")

(defun chk8 (/ retval)
  (setq	dwgname
	 (vl-registry-read
	   "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker"
	   "Current drawing"
	 )
  )
  (putinexcel "thisdwg" dwgname)

  (setq ssset (ssget "X" '((8 . "0"))))
  (if ssset
      (alert "No layer 0 found")
      (setq slen (sslength ssset))
  )
  (putinexcel "ZeroEnts" slen)



  (setq result (getresult "chk8"))
  (if (= result "x")
    (copytab "chk8results" "chk8")
  )
)

