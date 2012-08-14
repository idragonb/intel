(defun c:layer-tracker ()
   ;(command "OpenDCL")
   (Setq rValue (dcl_Project_Load (strcat installdirectory "Layer-manager") T)); remove T in publish
   (Setq rValue (dcl_Form_Show Layer-manager_Main-form))
   (init-main-form)

)

(defun init-main-form ()
   (setq data-dir-lyr "Q:\\X_CAD\\standard-checker\\")
   (init-layer-lists)
)


(defun init-layer-lists ()
   (dcl_ComboBox_Clear Layer-manager_Main-form_ComboBox1)
   (dcl_ComboBox_Clear Layer-manager_Main-form_ComboBox20)
   (setq first T)
   (while (setq tbllist (tblnext "LAYER" first))
      (setq thislayer (cdr (assoc 2 tbllist)))
      (setq first nil)
      (if (not (hasline thislayer))
         (if (isgoodlayer thislayer)
            (dcl_ComboBox_AddString Layer-manager_Main-form_ComboBox20 thislayer)
            (dcl_ComboBox_AddString Layer-manager_Main-form_ComboBox1 thislayer)
         )
      )
   )
   (if (listpopulated Layer-manager_Main-form_ComboBox20)
      (removefirstitem Layer-manager_Main-form_ComboBox20)
   )
   (if (listpopulated Layer-manager_Main-form_ComboBox1)
      (removefirstitem Layer-manager_Main-form_ComboBox1)
   )   
)

(defun listpopulated (thiscombo)
   (setq listleng (dcl_ComboBox_GetCount thiscombo))
   (setq retval nil)
   (if (> listleng 1)
      (setq retval T)
   )
   retval
)

(defun removefirstitem (thiscombo)
   (dcl_ComboBox_SetCurSel thiscombo 0)
)


(defun isgoodlayer (thislayer)
   (setq d_file (open (strcat data-dir-lyr "good-layers.txt") "r"))
   (setq retval nil)
   (while (setq r-line (read-line d_file))
      (if (= thislayer r-line)
         (setq retval T)
      )
   )
   (close d_file)
   retval
)
(defun do-init (fname comboname)
   (dcl_ComboBox_Clear comboname)
   (setq d_file (open (strcat data-dir-lyr fname) "r"))
   (while (setq r-line (read-line d_file))
      (dcl_ComboBox_AddString comboname r-line)
   )
   (close d_file)
)
;---------------buttons-------------------


(defun getfromshown ()
   (setq textline (dcl_Control_GetCaption Layer-manager_Main-form_Label16))
   (setq thislayer (substr textline 8))
;(alert thislayer)
   thislayer
)





(defun c:Layer-manager_Main-form_TextButton22_OnClicked ( /) ; remove from good layer list
   (Setq thisindex (dcl_ComboBox_GetCurSel Layer-manager_Main-form_ComboBox20))
   (Setq thislayer (dcl_ComboBox_GetLBText Layer-manager_Main-form_ComboBox20 thisindex))
   (removenotgoodlayer thislayer)
   (init-main-form)
)

(defun removenotgoodlayer (thislayer)
   (setq l_file (open (strcat data-dir-lyr "good-layers.txt") "r"))
   (setq t_file (open (strcat data-dir-lyr "temp.txt") "w"))
   (while (setq r-line (read-line l_file))
      (if (/= r-line thislayer)
         (write-line r-line t_file)
      )
   )
   (close l_file)
   (close t_file)
   (setq l_file (open (strcat data-dir-lyr "good-layers.txt") "w"))
   (setq t_file (open (strcat data-dir-lyr "temp.txt") "r"))
   (while (setq r-line (read-line t_file))
      (write-line r-line l_file)
   )
   (close l_file)
   (close t_file)
)   



(defun c:Layer-manager_Main-form_TextButton24_OnClicked ( /) ; refresh
     (init-main-form)
)
(defun c:Layer-manager_Main-form_TextButton17_OnClicked ( /) ; layer ok
   (Setq lindex (dcl_ComboBox_GetCurSel Layer-manager_Main-form_ComboBox1))
   (Setq mylayer (dcl_ComboBox_GetLBText Layer-manager_Main-form_ComboBox1 lindex))
   (putgoodlayer mylayer)
   (init-main-form)
)

(defun putgoodlayer (mylayer)
   (setq l_file (open (strcat data-dir-lyr "good-layers.txt") "a"))
   (write-line mylayer l_file)
   (close l_file)
)


;---------------layer view code------------
(defun c:Layer-manager_Main-form_TextButton26_OnClicked ( /) ; open layer view
     (Setq rValue (dcl_Form_Show Layer-manager_Form3 ))
)
(defun c:Layer-manager_Main-form_OnClose (nUpperLeftX nUpperLeftY /)
   (princ "\n-------Layer manager-----all rights reserved----ACT---0544832624---")
)

(defun c:Layer-manager_Main-form_TextButton23_OnClicked ( /) ; close
   (dcl_Form_CloseAll 0)
)



(defun c:Layer-manager_Form3_OnInitialize ( /)
   (setq first T)
   (while (setq tbllist (tblnext "LAYER" first))
      (setq thislayer (cdr (assoc 2 tbllist)))
      (setq first nil)
      (if (isgoodlayer thislayer)
         (princ)
         (if  (not (hasline thislayer))
            (dcl_ListBox_AddString Layer-manager_Form3_ListBox2 thislayer)
         )
      )
   )
)

(defun c:Layer-manager_Form3_ListBox2_OnDblClicked ( /)
   (Setq dblclickedindex (dcl_ListBox_GetCurSel Layer-manager_Form3_ListBox2))
   (Setq dblclickedlyr (dcl_ListBox_GetText Layer-manager_Form3_ListBox2 dblclickedindex))
   (setlayertoclicked dblclickedlyr)
   (turnoffalllayers)
   (turnonclickedlyr dblclickedlyr)
   (command "zoom" "e")
)
(defun setlayertoclicked (lyrname)
   (command "-layer" "set" lyrname "")
)
(defun turnoffalllayers ()
   (command "-layer" "off" "*" "y" "")
)
(defun turnonclickedlyr (lyrname)
   (command "-layer" "on" lyrname "")
)

(defun c:Layer-manager_Form3_TextButton6_OnClicked ( /)
   (turnonalllayers)
)
(defun turnonalllayers ()
   (command "-layer" "on" "*" "")
)


(defun hasline (str / slen nn retval curlet)
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
