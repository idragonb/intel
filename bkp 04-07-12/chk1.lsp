(load "excel utilities.lsp")

(defun chk1 (/ retval dwgname result)
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (putinexcel "thisdwg" dwgname)
  (setq result (getresult "chk1"))
  (if (= result "x")
    (copytab "chk1results" "chk1")
  )
 ;(putresult nn result)
)

(defun putresult (nn result / excel range)
  (setq excel (vlax-get-object "excel.application"))
  (setq range (vlax-get-property excel 'Range "dwgname"))
  (vlax-put-property range 'Item (1+ nn) 2 result)
)

  

  

  

