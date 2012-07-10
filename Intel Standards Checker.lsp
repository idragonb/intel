(setq installdirectoryINTEL (strcat instdirINTEL "\\"))
(load "excel utilities.lsp")

(defun c:Intel_Checker ()
   ;(open-dwg-prototype) ; so that it will not close at program termination
   (command "OpenDCL")
   (Setq rValue (dcl_Project_Load (strcat installdirectoryINTEL "Intel-Standards-Checker") ));T
   (if (not (dcl_Form_IsActive Intel-Standards-Checker_Form1))
      (progn
         (Setq rValue (dcl_Form_Show Intel-Standards-Checker_Form1))
      )
   )
)
;---------- set prototype in background
(defun open-dwg-prototype ()
   (setq acad (vlax-get-acad-object))
   (setq docs (vlax-get-property acad 'Documents))
   ; check each item in loop if name = prototype
      ; if yes set
      ; if no open
      (setq protofile (strcat installdirectoryINTEL "prototype.dwg"))
      (setq doc (vlax-invoke-method docs 'Open protofile))
      (vlax-invoke-method doc 'Activate)
)
   
   
;--------- help controls

(defun c:Intel-Standards-Checker_Form1_PictureBox26_OnMouseEntered ( /)
   (dcl_Control_SetVisible Intel-Standards-Checker_Form1_TextBox38 T)
)

(defun c:Intel-Standards-Checker_Form1_PictureBox26_OnMouseMovedOff ( /)
   (dcl_Control_SetVisible Intel-Standards-Checker_Form1_TextBox38 F)
)

(defun c:Intel-Standards-Checker_Form1_PictureBox26_OnClicked ( /)
     (Setq rValue (dcl_Form_Show Intel-Standards-Checker_Form5))
)



;--------- initialize


(defun c:Intel-Standards-Checker_Form1_OnInitialize ( /)
   (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_TextButton6 nil)

   (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_TextButton10 nil)
   (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_TextButton11 nil)

   (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_TextButton15 nil)
   (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_TextButton3 nil)

   (dcl_ListBox_Clear Intel-Standards-Checker_Form1_ListBox7);4/7
 ;  (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_CheckBox3 nil)

)

;----------- count files

(defun c:Intel-Standards-Checker_Form1_TextButton30_OnClicked ( /)
    (Setq number-files-shown (dcl_ListBox_GetCount Intel-Standards-Checker_Form1_ListBox7))
    (setq str-count (itoa number-files-shown))
    (dcl_Control_SetCaption Intel-Standards-Checker_Form1_Label31 str-count)
)


;--------open current directory

(defun c:Intel-Standards-Checker_Form1_TextButton35_OnClicked ( /)
   (startapp (strcat "explorer /e," (dcl_Control_GetCaption Intel-Standards-Checker_Form1_Label5)))
)


(defun cdate-str (date-real / yr mon day hr mnt sec hun)
    (setq yr (fix (/ date-real 10000))
          date-real (rem date-real 10000)
          mon (fix (/ date-real 100))
          date-real (rem date-real 100)
          day (fix date-real)
          date-real (* (- date-real day) 1e9)
          hr (fix (/ date-real 1e7))
          date-real (rem date-real 1e7)
          mnt (fix (/ date-real 1e5))
          date-real (rem date-real 1e5)
          sec (fix (/ date-real 1000))
          hun (fix (rem date-real 100)))

    (strcat (itoa day) "." (itoa mon) "." (itoa yr) " "
            (itoa hr) ":" (itoa mnt)   );  ":" (itoa sec) "," (itoa hun))
)


;--------- report controls

; layer manager

(defun add-to-list (rline this-list / nn)
   (setq nn 1)
   (while (/= (substr rline nn 4) ".dwg")
      (setq nn (1+ nn))
   )
   (setq this-dwg (substr rline 1 (+ nn 3)))
   (dcl_ListBox_AddString this-list this-dwg)
)

; -------- script controls

(defun put-file-in-list (rline / nn)
   (setq rlength (strlen rline))
   (setq nn rlength)
   (while (/= (substr rline nn 1) "\\")
      (setq nn (1- nn))
   )
   (setq this-filename (substr rline (+ nn 1) (- (- rlength nn) 1)))
   (dcl_ListBox_AddString Intel-Standards-Checker_Form1_ListBox7 this-filename)
)

(defun make-excel-name ()
   (strcat "Intel Standard Report " (rtos (getvar "cdate") 2 4) ".xls")
)

(defun link-to-excel ()
  ; set pathname to read from registry
  (Setq path-name (dcl_Control_GetCaption Intel-Standards-Checker_Form1_Label5))
  (vl-registry-write "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Working directory" path-name)
  
  (setq excel (vlax-get-or-create-object "Excel.Application"))
  (setq GLOBAL-excel excel)
  (setq workbooks (vlax-get-property excel 'Workbooks))
  (setq workbook (vlax-invoke-method workbooks 'Open (strcat installdirectoryINTEL "Prototype.xls")))
  (setq GLOBAL-excel-workbook workbook)
  (setq reportname (make-excel-name))
  (vlax-invoke-method workbook 'Saveas (strcat path-name reportname) -4143 nil nil :vlax-false :vlax-false 1 2)
  (vla-put-visible excel :vlax-true)
  (setq xlsheets (vlax-get-property workbook 'Sheets))
  (setq xlsheet (vlax-get-property xlsheets 'Item "extractions"))
  (vlax-invoke-method xlsheet 'Activate)
  (setq range (vlax-get-property xlsheet 'Range "Date"))
  (vlax-put-property range 'Item 1 1 (exceldate))
)



(defun c:Intel-Standards-Checker_Form1_TextButton15_OnClicked ( / nn) ; run checks

  (Setq path-name (dcl_Control_GetCaption Intel-Standards-Checker_Form1_Label5))
  (vl-registry-write "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Working directory" path-name)

  (Setq list-length (dcl_ListBox_GetCount Intel-Standards-Checker_Form1_ListBox7))
  (setq nn 0)

  (link-to-excel)
     
  (while (< nn list-length)
    (Setq list-item (dcl_ListBox_GetText Intel-Standards-Checker_Form1_ListBox7 nn))
    (vl-registry-write "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing" list-item)
    (do-checks nn)
    (setq nn (1+ nn))
    (put-summary nn)
  )
  (dcl_Control_SetCaption Intel-Standards-Checker_Form1_Label1 reportname)

  (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_TextButton3 T)


 ; (setq excel (vlax-get-or-create-object "Excel.Application"))
 ; (setq workbooks (vlax-get-property excel 'Workbooks))
 ; (setq xlsheets (vlax-get-property workbook 'Sheets))
 ; (setq xlsheet (vlax-get-property xlsheets 'Item "Summary"))
 ; (vlax-invoke-method xlsheet 'Activate)
  (terpri)
  (terpri)
  (princ "Completed checks - view excel...")
)

(defun put-summary (nn)
  (setq dname (getresult "thisdwg"))
  (putinexcel-indexed "dwgchecked" dname nn 1)
  (setq r1 (getresult "chk1"))
  (putinexcel-indexed "chk1Summary" r1 nn 1)
  (setq r2 (getresult "chk2"))
  (putinexcel-indexed "chk2Summary" r2 nn 1)
  (setq r3 (getresult "chk3"))
  (putinexcel-indexed "chk3Summary" r3 nn 1)
  (setq r4 (getresult "chk4"))
  (putinexcel-indexed "chk4Summary" r4 nn 1)
  (setq r5 (getresult "chk5"))
  (putinexcel-indexed "chk5Summary" r5 nn 1)
  (setq r6 (getresult "chk6"))
  (putinexcel-indexed "chk6Summary" r6 nn 1)
  (setq r7 (getresult "chk7"))
  (putinexcel-indexed "chk7Summary" r7 nn 1)
  (setq r8 (getresult "chk8"))
  (putinexcel-indexed "chk8Summary" r8 nn 1)
  (setq r9 (getresult "chk9"))
  (putinexcel-indexed "chk9Summary" r9 nn 1)
  (setq r10 (getresult "chk10"))
  (putinexcel-indexed "chk10Summary" r10 nn 1)
  (setq r11 (getresult "chk11"))
  (putinexcel-indexed "chk11Summary" r11 nn 1)
  (setq r12 (getresult "chk12"))
  (putinexcel-indexed "chk12Summary" r12 nn 1)
  (setq r13 (getresult "chk13"))
  (putinexcel-indexed "chk13Summary" r13 nn 1)
  (setq r14 (getresult "chk14"))
  (putinexcel-indexed "chk14Summary" r14 nn 1)
  (setq r15 (getresult "chk15"))
  (putinexcel-indexed "chk15Summary" r15 nn 1)
  (setq r16 (getresult "chk16"))
  (putinexcel-indexed "chk16Summary" r16 nn 1)
  (setq r17 (getresult "chk17"))
  (putinexcel-indexed "chk17Summary" r17 nn 1)
  
 ; (setq r18 (getresult "chk18"))
 ; (putinexcel-indexed "chk18Summary" r18 nn 1)
)


(defun c:Intel-Standards-Checker_Form1_TextButton3_OnClicked (/)
  (setq excel (vlax-get-or-create-object "Excel.Application"))
  (setq workbooks (vlax-get-property excel 'Workbooks))
  (Setq path-name (dcl_Control_GetCaption Intel-Standards-Checker_Form1_Label5))
  (setq reportname (dcl_Control_GetCaption Intel-Standards-Checker_Form1_Label1))
  (setq workbook (vlax-invoke-method workbooks 'Open (strcat path-name reportname)))
  (vla-put-visible excel :vlax-true)
  (vlax-invoke-method workbook 'Activate)
  (vlax-release-object workbook)
  (vlax-release-object workbooks)
  (vlax-release-object excel)
)


(defun exceldate (/ cdate year month day hour minutes)
  (setq cdate (rtos (getvar "cdate")))
  (setq year (substr cdate 1 4))
  (setq month (substr cdate 5 2))
  (setq day (substr cdate 7 2))
  (setq hour (substr cdate 10 2))
  (setq minutes (substr cdate 12 2))
  (strcat day "-" month "-" year)
)

(defun open-dwg-and-link (docfile)
  (setq acadmain (vlax-get-acad-object))
  (setq alldocs (vlax-get-property acadmain 'Documents))
  (vlax-invoke-method alldocs 'Open docfile :vlax-true) ; open read-only
 )

(defun do-checks (nn / docfile acadmain alldocs startdoc currdoc)

  (setq dwgpath (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Working directory"))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq docfile (strcat dwgpath dwgname))
   
  (setq currdoc (open-dwg-and-link docfile)) ; open read-only
  (load "extract-all.lsp") ; load in header
  (dump-to-excel)

  (copytab "copyarea" "extractions")

  (vlax-invoke-method currdoc 'Close)
  (princ docfile)
  (terpri)
)

;(defun dwgname-excel (dwgname nn / excel workbooks workbook xlsheet range)
;  (setq excel (vlax-get-or-create-object "Excel.Application"))
;  (setq workbooks (vlax-get-property excel 'Workbooks))
;  (setq workbook (vlax-get-property excel 'ActiveWorkbook))
;  (setq xlsheet (vlax-get-property excel 'ActiveSheet))
;  (setq range (vlax-get-property excel 'Range "dwgname"))
;  (vlax-put-property range 'Item (+ nn 1) 1 dwgname);
;
;  (setq result (getresult "chk1"))
;  (vlax-put-property range 'Item (+ nn 1) 2 result)
;  (setq result (getresult "chk2"))
;  (vlax-put-property range 'Item (+ nn 1) 3 result)
;    (setq result (getresult "chk3"))
;  (vlax-put-property range 'Item (+ nn 1) 4 result)
;    (setq result (getresult "chk4"))
;  (vlax-put-property range 'Item (+ nn 1) 5 result)
;    (setq result (getresult "chk5"))
;  (vlax-put-property range 'Item (+ nn 1) 6 result)
;    (setq result (getresult "chk6"))
;  (vlax-put-property range 'Item (+ nn 1) 7 result)
;  ;
;
;  (vlax-release-object range)
;  (vlax-release-object xlsheet)
;  (vlax-release-object workbook)
;  (vlax-release-object workbooks)
;  (vlax-release-object excel);

;)


  



(defun subin (str sub-str str-iden / nn retval)
   (setq nn 1)
   (setq s-len (strlen str-iden))
   (while (/= str-iden (setq aaa (substr str nn s-len)))
 ;     (princ aaa)
;(getstring "\n")
      (setq nn (1+ nn))
   )
   (setq retval (strcat (substr str 1 (- nn 1)) sub-str (substr str (+ nn s-len))))
)

       
(defun hasin (str sub-str)
   (if str
      (progn
         (setq matchchk (strcat "*" sub-str "*"))
         (wcmatch str matchchk)
      )
      nil
   )
)
         


;-----------choose files buttons

(defun c:Intel-Standards-Checker_Form1_TextButton1_OnClicked ( / path-name) ; Browse button
   ;(Setq path-name (dcl_Control_GetCaption Intel-Standards-Checker_Form1_Label5))
   (Setq path-name(vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Last directory"))
   (Setq path-name (dcl_BrowseFolder "Choose the working directory to begin checking..." path-name))
   (vl-registry-write "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Last directory" path-name)
   (if path-name
      (progn
         (dcl_Control_SetCaption Intel-Standards-Checker_Form1_Label5 (strcat path-name "\\"))
         (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_TextButton6 T)

    
      )
   )
)

(defun c:Intel-Standards-Checker_Form1_TextButton6_OnClicked ( / path-name dwg-list) ; add files button
   (Setq path-name (dcl_Control_GetCaption Intel-Standards-Checker_Form1_Label5))
   (setq path-len (+ (strlen path-name) 1))
   (Setq dwg-list (dcl_MultiFileDialog  (list "Autocad Files|*.dwg" "From Text File|*.txt") "Choose drawings to add to checks" path-name))
   (if dwg-list
      (put-dwg-list dwg-list)
   )
)
(defun put-dwg-list (dwg-list)
   (setq first-item (substr (nth 0 dwg-list) path-len))
   (setq extension (substr first-item (- (strlen first-item) 2)))
   (if (= extension "dwg")
      (foreach thisdwg dwg-list
         (dcl_ListBox_AddString Intel-Standards-Checker_Form1_ListBox7 (substr thisdwg path-len))
      )
   )
   (if (= extension "txt")
      (progn
         (setq s_file (open (nth 0 dwg-list) "r"))
         (while (setq thisline (read-line s_file))
            (dcl_ListBox_AddString Intel-Standards-Checker_Form1_ListBox7 (substr thisline path-len))
         )
         (close s_file)
      )
   )

   (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_TextButton10 T)
   (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_TextButton11 T)
   (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_TextButton15 T)
)


(defun c:Intel-Standards-Checker_Form1_TextButton10_OnClicked ( /)  ; remove selected
   (Setq rem-list (dcl_ListBox_GetSelectedNths Intel-Standards-Checker_Form1_ListBox7))
   (setq list-length (length rem-list))
   (while (> list-length 0)
      (setq list-length (1- list-length))
      (Setq this-item (nth list-length rem-list))
      (dcl_ListBox_DeleteString Intel-Standards-Checker_Form1_ListBox7 this-item)
   )
)

(defun c:Intel-Standards-Checker_Form1_TextButton11_OnClicked ( /) ; clear all
   (dcl_ListBox_Clear Intel-Standards-Checker_Form1_ListBox7)

   (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_TextButton10 nil)
   (dcl_Control_SetEnabled Intel-Standards-Checker_Form1_TextButton11 nil)
)


;------------------------