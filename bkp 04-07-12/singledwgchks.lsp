(defun c:singledwgchks ()
   (command "OpenDCL")
 ;  (Setq rValue (dcl_LoadProject "Singledwgchks" T))
   (Setq rValue (dcl_Project_Load "Singledwgchks" T))
   (Setq rValue (dcl_Form_Show Singledwgchks_Form1))
   (load "Q:\\X_CAD\\standard-checker\\submittalchk.lsp")
   (setq str (getreportstring))
   (loadinterface str)
)

(defun loadinterface (str)
   (setq dwgnamestr (getdata 1 str))
   (dcl_Form_SetTitleBarText Singledwgchks_Form1 (strcat "Drawing being checked: " dwgnamestr))
   (setq chkboxstr1 (getdata 2 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label27 chkboxstr1)
   (setq chkboxstr2 (getdata 3 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label26 chkboxstr2)
   (setq chkboxstr3 (getdata 4 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label25 chkboxstr3)
   (setq chkboxstr4 (getdata 5 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label24 chkboxstr4)
   (setq chkboxstr5 (getdata 6 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label23 chkboxstr5)
   (setq chkboxstr6 (getdata 7 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label22 chkboxstr6)
   (setq chkboxstr7 (getdata 8 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label21 chkboxstr7)
   (setq chkboxstr8 (getdata 9 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label20 chkboxstr8)
   (setq chkboxstr9 (getdata 10 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label19 chkboxstr9)
   (setq chkboxstr10 (getdata 11 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label18 chkboxstr10)
   (setq chkboxstr11 (getdata 12 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label17 chkboxstr11)
   (setq chkboxstr12 (getdata 13 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label16 chkboxstr12)
   (setq chkboxstr13 (getdata 14 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label15 chkboxstr13)
   (setq chkboxstr14 (getdata 15 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label28 chkboxstr14)
   (setq chkboxstr15 (getdata 16 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label29 chkboxstr15)
   (setq chkboxstr16 (getdata 17 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label30 chkboxstr16)
   (setq chkboxstr17 (getdata 18 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label31 chkboxstr17)
   (setq chkboxstr18 (getdata 19 str))
   (dcl_Control_SetCaption Singledwgchks_Form1_Label32 chkboxstr18)
   (setq chkboxstr20 (getdata 21 str))
   (dcl_Control_SetText Singledwgchks_Form1_TextBox14 chkboxstr20)
)

(defun getdata (nn str / counter)
   (setq counter 0)
   (setq mm 1)
   (setq retval "")
   (setq slen (strlen str))
   (while (and (< counter nn) (< mm slen))
      (if (= thislet "^")
         (setq retval "")
      )
      (setq thislet (substr str mm 1))
      (if (= thislet "^")
         (setq counter (1+ counter))
         (setq retval (strcat retval thislet))
      )
      (setq mm (1+ mm))
   )
   retval
)