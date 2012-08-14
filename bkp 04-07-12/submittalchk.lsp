(defun c:submittalchk ()
   (dcl_Form_Show Intel-Standards-Checker_Form2)
   (setq reportstring (getreportstring))
   (reporttofile reportstring)
)

(defun c:getreportstring ()
   (setq reportstring "")
   (getreportstring)
)

(defun checkoff (boxname)
   (dcl_Control_SetValue boxname 1)
   (dcl_Control_SetFontStrikeout boxname T)
   (dcl_Control_ForceUpdateNow boxname)
  ; (dcl_Form_Close Intel-Standards-Checker_Form2)
  ; (dcl_Form_Show Intel-Standards-Checker_Form2)
)

(defun cleanlist (boxname)
   (dcl_Control_SetValue boxname 0)
   (dcl_Control_SetFontStrikeout boxname nil)
   (dcl_Control_ForceUpdateNow boxname)
)

(defun getreportstring (/ str)
   (setq generalnotes nil)
   (setq reportstring "")


   (cleanlist Intel-Standards-Checker_Form2_CheckBox2)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox3)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox4)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox5)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox6)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox7)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox8)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox9)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox10)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox11)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox12)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox13)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox14)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox15)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox16)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox17)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox18)
   (cleanlist Intel-Standards-Checker_Form2_CheckBox19)



   (setq chk1str (chk1))
(checkoff Intel-Standards-Checker_Form2_CheckBox2)
(princ "\nCheck 1 completed successfully...")
   (setq chk2str (chk2))
(checkoff Intel-Standards-Checker_Form2_CheckBox3)
(princ "\nCheck 2 completed successfully...")
   (setq chk3str (chk3))
(checkoff Intel-Standards-Checker_Form2_CheckBox4)
(princ "\nCheck 3 completed successfully...")
   (setq chk4str (chk4))
(checkoff Intel-Standards-Checker_Form2_CheckBox5)
   (setq chk5str (chk5))
(princ "\nCheck 4 completed successfully...")
(checkoff Intel-Standards-Checker_Form2_CheckBox6)
(princ "\nCheck 5 completed successfully...")
   (setq chk6str (chk6))
(checkoff Intel-Standards-Checker_Form2_CheckBox7)
(princ "\nCheck 6 completed successfully...")
   (setq chk7str (chk7))
(checkoff Intel-Standards-Checker_Form2_CheckBox8)
(princ "\nCheck 7 completed successfully...")
   (setq chk8str (chk8))
(checkoff Intel-Standards-Checker_Form2_CheckBox9)
(princ "\nCheck 8 completed successfully...")
   (setq chk9str (chk9))
(checkoff Intel-Standards-Checker_Form2_CheckBox10)
(princ "\nCheck 9 completed successfully...")
   (setq chk10str (chk10))
(checkoff Intel-Standards-Checker_Form2_CheckBox11)
(princ "\nCheck 10 completed successfully...")
   (setq chk11str (chk11))
(checkoff Intel-Standards-Checker_Form2_CheckBox12)
(princ "\nCheck 11 completed successfully...")
   (setq chk12str (chk12))
(checkoff Intel-Standards-Checker_Form2_CheckBox13)
(princ "\nCheck 12 completed successfully...")
   (setq chk13str (chk13))
(checkoff Intel-Standards-Checker_Form2_CheckBox14)
(princ "\nCheck 13 completed successfully...")
   (setq chk14str (chk14))
(checkoff Intel-Standards-Checker_Form2_CheckBox15)
(princ "\nCheck 14 completed successfully...")
   (setq chk15str (chk15))
(checkoff Intel-Standards-Checker_Form2_CheckBox16)
(princ "\nCheck 15 completed successfully...")
   (setq chk16str (chk16))
(checkoff Intel-Standards-Checker_Form2_CheckBox17)
(princ "\nCheck 16 completed successfully...")
   (setq chk17str (chk17))
(checkoff Intel-Standards-Checker_Form2_CheckBox18)
(princ "\nCheck 17 completed successfully...")
   (setq chk18str (chk18))
(checkoff Intel-Standards-Checker_Form2_CheckBox19)
(princ "\nCheck 18 completed successfully...")







(princ "\nReportstring is: ")
(princ reportstring)
(terpri)

   (setq reportstring
      (strcat
         (getvar "dwgname") "^"
         chk1str "^"
         chk2str "^"
         chk3str "^"
         chk4str "^"
         chk5str "^"
         chk6str "^"
         chk7str "^"
         chk8str "^"
         chk9str "^"
         chk10str "^"
         chk11str "^"
         chk12str "^"
         chk13str "^"
         chk14str "^"
         chk15str "^"
         chk16str "^"
         chk17str "^"
         chk18str "^"
         reportstring
      )
   )
  ; (reporttofile reportstring)
   reportstring
)

;-------report to file



(defun reporttofile (str)

;   (setq i_file (open "C:\\_Vbasic\\thiscomp.txt" "r"))
;   (setq thisuser (read-line i_file))
;   (close i_file)
;   (setq tsname (strcat "P:\\Lisps\\DATABASE\\excelreport" thisuser ".txt"))
(Setq path-name (dcl_Control_GetCaption Intel-Standards-Checker_Form1_Label5))
(setq report-name (dcl_Control_GetCaption Intel-Standards-Checker_Form1_Label20))
(Setq tsname (strcat path-name "\\" report-name))

   (setq r_file (open tsname "a"))
   (write-line str r_file)
   (close r_file)
  ; (princ "Open excelreport.txt in DATABASE directory with excel and ^ separator to see report")
)



;------chk18---
(load (strcat installdirectory "chk18.lsp"))

;------chk17---
(load (strcat installdirectory "chk17.lsp"))

;------chk16---
(load (strcat installdirectory "chk16.lsp"))
   
;------chk15---
(load (strcat installdirectory "chk15.lsp"))

;------chk14---
(load (strcat installdirectory "chk14.lsp"))
;------chk13---
(load (strcat installdirectory "chk13.lsp"))

;-------chk12---
(load (strcat installdirectory "chk12.lsp"))
;-------chk11---
(load (strcat installdirectory "chk11.lsp"))

;-------chk10---
(load (strcat installdirectory "chk10.lsp"))

;-------chk9---
(load (strcat installdirectory "chk9.lsp"))

;-------chk8---
(load (strcat installdirectory "chk8.lsp"))

;-------chk7---
(load (strcat installdirectory "chk7.lsp"))
;-------chk6---
(load (strcat installdirectory "chk6.lsp"))

;-------chk5---
(load (strcat installdirectory "chk5.lsp"))
;-------chk4---

(load (strcat installdirectory "chk4.lsp"))
;-------chk3---
(load (strcat installdirectory "chk3.lsp"))

;-------chk2---

(load (strcat installdirectory "chk2.lsp"))
;-------chk1---

(load (strcat installdirectory "chk1.lsp"))




;(defun hasline (str / retval)
;   (setq slen (strlen str))
;   (setq nn 0)
;   (setq retval nil)
;   (while (< nn slen)
;      (setq nn (1+ nn))
;      (setq curlet (substr str nn 1))
;      (if (= curlet "|")
;         (setq retval T)
;      )
;   )
;   retval
;)

(defun hasline (str / retval)
   (wcmatch str "*|*")
)


;-------
