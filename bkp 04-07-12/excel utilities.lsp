(defun copytab (rangename tabname / excel range sheets newsheet cells)
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq excel (vlax-get-object "excel.application"))
  (setq range (vlax-get-property excel 'Range rangename))
  (setq sheets (vlax-get-property excel 'Sheets))
  (setq newsheet (vlax-invoke-method sheets 'Add))
  (vlax-put-property newsheet 'Name (strcat dwgname "-" tabname))
  (vlax-invoke-method range 'Copy)
  (setq cells (vlax-get-property newsheet 'Cells))
  (vlax-invoke-method cells 'PasteSpecial -4163 -4142 :vlax-false :vlax-false)
)

(defun putinexcel (rangename content / excel workbook range)
 ; (putinexcel-indexed rangename content 1 1)
  (setq excel (vlax-get-object "excel.application"))
  (setq workbook (vlax-get-property excel 'ActiveWorkbook))
  (setq range (vlax-get-property excel 'Range rangename))
  (vlax-put-property range 'Item 1 1 content)
)

(defun getresult (chkrange / excel range)
  (setq excel (vlax-get-object "excel.application"))
  (setq range (vlax-get-property excel 'Range chkrange))
  (vlax-variant-value (vlax-get-property range 'Text))
)
(defun putinexcel-indexed (rangename content row column / excel workbook range)
  (setq excel (vlax-get-object "excel.application"))
  (setq workbook (vlax-get-property excel 'ActiveWorkbook))
  (setq range (vlax-get-property excel 'Range rangename))
  (vlax-put-property range 'Item row column content)
)