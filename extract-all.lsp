(load "excel utilities.lsp")

(defun dump-to-excel ()
   (chk1-extractions)
   (chk2-extractions)
   (chk3-5-6-7-extractions)
   (chk8-extractions)
   (chk9-extractions)
   (chk10-extractions)
   (chk11-extractions)
   (chk12-extractions)
   (chk14-extractions)
   (chk15-extractions)
   (chk16-extractions)
   (chk17-extractions)
   (chk18-extractions)

)



(defun chk1-extractions ()
   ;--- chk1 extraction
   (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
   (putinexcel "thisdwg" dwgname)
   (princ "\nDrawing name ")
   (princ dwgname)
   (princ " sent to excel...")
)

(defun chk2-extractions ()
   ; --- chk2 extraction
   (setq count (blkcount "I1184sht" "ModelSpace")) ; # of title blocks in model
   (putinexcel "titleinmodel" (itoa count))
   (setq count (blkcount "I1184sht" "PaperSpace")) ; # of title blocks in paper
   (putinexcel "titleinpaper" (itoa count))
   (if (= count 1)
      (progn
         (setq obj (getblk "I1184sht" "PaperSpace")) ; if only once in paper - get obj
         (setq blkid (vlax-get-property obj 'ObjectID))
         (setq blklyr (vlax-get-property obj 'Layer))
         (putinexcel "titlelayer" blklyr)
      
         (setq titleatts (getatts blkid "PaperSpace"))

         (putinexcel "OldNum" (nth 1 (assoc "OLD-NUM" titleatts)))
         (putinexcel "Fwr" (nth 1 (assoc "FWR" titleatts)))
         (putinexcel "Rev" (nth 1 (assoc "##__##" titleatts)))
         (putinexcel "PlotScale" (nth 1 (assoc "PLOTSCALE" titleatts)))
         (putinexcel "Scale" (nth 1 (assoc "SCALE" titleatts)))
         (putinexcel "Title4" (nth 1 (assoc "TITLE-4" titleatts)))
         (putinexcel "Title3" (nth 1 (assoc "TITLE-3" titleatts)))
         (putinexcel "Title2" (nth 1 (assoc "TITLE-2" titleatts)))
         (putinexcel "Title1" (nth 1 (assoc "TITLE-1" titleatts)))
         (putinexcel "File" (nth 1 (assoc "FILE" titleatts)))
         (putinexcel "DwgNum" (nth 1 (assoc "DWG#" titleatts)))
         (putinexcel "DwgDate" (nth 1 (assoc "DRW_DATE" titleatts)))
         (putinexcel "CheckDate" (nth 1 (assoc "CHK_DATE" titleatts)))
         (putinexcel "CrsDate" (nth 1 (assoc "CRS_DATE" titleatts)))
         (putinexcel "SafDate" (nth 1 (assoc "SAF_DATE" titleatts)))
         (putinexcel "PlaDate" (nth 1 (assoc "PLA_DATE" titleatts)))
         (putinexcel "EngDate" (nth 1 (assoc "ENG_DATE" titleatts)))
         (putinexcel "AppDate" (nth 1 (assoc "APP_DATE" titleatts)))
         (putinexcel "Drawn" (nth 1 (assoc "DRAWN" titleatts)))
         (putinexcel "Checked" (nth 1 (assoc "CHECKED" titleatts)))
         (putinexcel "CrossCheck" (nth 1 (assoc "CROSSCHECK" titleatts)))
         (putinexcel "Safety" (nth 1 (assoc "SAFETY" titleatts)))
         (putinexcel "Planning" (nth 1 (assoc "PLANNING" titleatts)))
         (putinexcel "Engineer" (nth 1 (assoc "ENGINEER" titleatts)))
         (putinexcel "Approved" (nth 1 (assoc "APPROVED" titleatts)))
         (putinexcel "EqtCode" (nth 1 (assoc "EQT_CODE" titleatts)))
      )
    )
)

(defun chk3-5-6-7-extractions ()
   (listxrefs)
)

(defun chk8-extractions ()
   (setq lyr0ents (+ (layer0sum "ModelSpace") (layer0sum "PaperSpace")))
   (putinexcel "layer0ents" lyr0ents) 
)

(defun chk10-extractions ()
   (setq papertext (objsum "AcDbText" "PaperSpace"))
   (putinexcel "papertext" papertext)
   (setq paperblks (objsum "AcDbBlockReference" "PaperSpace"))
   (putinexcel "paperblks" paperblks)
   (setq paperlines (objsum "AcDbLine" "PaperSpace"))
   (putinexcel "paperlines" paperlines)
   (setq papercircles (objsum "AcDbCircle" "PaperSpace"))
   (putinexcel "papercircles" papercircles)
   (setq paperall (objsum "all" "PaperSpace"))
   (putinexcel "paperall" paperall)
   (setq modeltext (objsum "AcDbText" "ModelSpace"))
   (putinexcel "modeltext" modeltext)
   (setq modelblks (objsum "AcDbBlockReference" "ModelSpace"))
   (putinexcel "modelblks" modelblks)
   (setq modellines (objsum "AcDbLine" "ModelSpace"))
   (putinexcel "modellines" modellines)
   (setq modelcircles (objsum "AcDbCircle" "ModelSpace"))
   (putinexcel "modelcircles" modelcircles)
   (setq modelall (objsum "all" "ModelSpace"))
   (putinexcel "modelall" modelall)
)
(defun chk11-extractions ()
   ;-----------------chk 11 text styles
   (dumpstyles)
   ; dump text heights
   (dumptext)
   
)

(defun chk12-extractions ()
   ;-----------------chk 12 block names
   (dumpblocks)
)

(defun chk14-extractions ()
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  
  (setq blocks (vlax-get-property doc 'Blocks))
  (setq nn 0)
  (setq nqty 0)
  (setq blkrot nil)
  (while (< nn (vlax-get-property blocks 'Count))
     (setq curblk (vlax-invoke-method blocks 'Item nn))
     (setq blkname (vlax-get-property curblk 'Name))
     (if (= blkname "G-INORTH")
        (progn
           (setq paperspace (vlax-invoke-method blocks 'Item "*PAPER_SPACE"))
           (setq pssize (vlax-get-property paperspace 'Count))
           (setq mm 0)
           (while (< mm pssize)
              (setq obj (vlax-invoke-method paperspace 'Item mm))
              (setq objname (vlax-get-property obj 'ObjectName))
              (if (= objname "AcDbBlockReference")
                 (setq blkname (vlax-get-property obj 'Name))
                 (setq blkname "")
              )
              (if (= blkname "G-INORTH")
                 (progn
                    (setq blkrot (vlax-get-property obj 'Rotation))
                    (setq nqty (1+ nqty))
                  )
               )
               (setq mm (1+ mm))
            )
           (putinexcel "northarrowqty" nqty)
           (putinexcel "northarrowangle" blkrot)
         )
      )
      (setq nn (1+ nn))
   )
)

(defun chk15-extractions ()
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  
  (setq blocks (vlax-get-property doc 'Blocks))
  (setq nn 0)
  (setq nqty 0)

  (while (< nn (vlax-get-property blocks 'Count))
     (setq curblk (vlax-invoke-method blocks 'Item nn))
     (setq blkname (vlax-get-property curblk 'Name))
     (if (= blkname "IBLK_1")
        (progn
           (setq paperspace (vlax-invoke-method blocks 'Item "*PAPER_SPACE"))
           (setq pssize (vlax-get-property paperspace 'Count))
           (setq mm 0)
           (while (< mm pssize)
              (setq obj (vlax-invoke-method paperspace 'Item mm))
              (setq objname (vlax-get-property obj 'ObjectName))
              (if (= objname "AcDbBlockReference")
                 (setq blkname (vlax-get-property obj 'Name))
                 (setq blkname "")
              )
              (if (= blkname "IBLK_1")
                 (progn
                    (setq inspt (vlax-get-property obj 'InsertionPoint));********variant
                    (setq ptval (vlax-variant-value inspt))
                    (setq borg (vlax-safearray->list ptval))
                    (setq eorg (strcat (rtos (nth 0 borg)) ; turn coords to string
				                   ","
				                 (rtos (nth 1 borg))
				                     ","
				                 (rtos (nth 2 borg))
			                   )
	                  )
	                  (putinexcel-indexed "revisionline" eorg (1+ nqty) 0)
	                    	
                    (setq lyr (vlax-get-property obj 'Layer)); putinexcel
                    (setq atts (vlax-invoke-method obj 'GetAttributes))
                    (setq attsarray (vlax-variant-value atts))
                    (setq len (1+ (vlax-safearray-get-u-bound attsarray 1)))
                    (setq rr 0)
                    (while (< rr len)
                       (setq att (vlax-safearray-get-element attsarray rr))
                       (setq tag (vlax-get-property att 'TagString))
                       (setq val (vlax-get-property att 'TextString))
                       (setq rr (1+ rr))
                       (putinexcel-indexed "revisionline" val (1+ nqty) rr)
                     )
                    
                    (setq nqty (1+ nqty))
                  )
               )
               (setq mm (1+ mm))
            )
         )
      )
      (setq nn (1+ nn))
   )
)

(defun chk16-extractions ()
   ;----------------chk 16 dim styles
   (dumpdims)
   ;need entity scan
   (dumpdiments)
)


;dimEntities
;--------------------------------
(defun dumpdiments()
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  (setq tt 0)
  
  (setq tt (dimspacedump doc "*MODEL_SPACE" tt))
  (dimspacedump doc "*PAPER_SPACE" tt)
)

(defun dimspacedump (doc curspace tt)
  (setq blocks (vlax-get-property doc "Blocks"))
  (setq modelspace (vlax-invoke-method blocks 'Item curspace))
  (setq count (vlax-get-property modelspace 'Count))
  (setq nn 0)
  (while (< nn count)
     (setq obj (vlax-invoke-method modelspace 'Item nn))
     (if  (or
            (= "AcDbRotatedDimension" (vlax-get-property obj 'ObjectName))
            (= "AcDb2LineAngularDimension" (vlax-get-property obj 'ObjectName))
            (= "AcDb3PointAngularDimension" (vlax-get-property obj 'ObjectName))
            (= "AcDbAlignedDimension" (vlax-get-property obj 'ObjectName))
            (= "AcDbArcDimension" (vlax-get-property obj 'ObjectName))
            (= "AcDbDiametricDimension" (vlax-get-property obj 'ObjectName))
            (= "AcDbOrdinateDimension" (vlax-get-property obj 'ObjectName))
            (= "AcDbRadialDimension" (vlax-get-property obj 'ObjectName))
            (= "AcDbRadialDimensionLarge" (vlax-get-property obj 'ObjectName))
          )
        (progn
          (setq tt (1+ tt))
           
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'DecimalSeparator) tt 1)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'DimensionLineColor) tt 2)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'DimensionLineExtend) tt 3)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'DimensionLinetype) tt 4)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'ExtensionLineColor) tt 5)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'ExtensionLineExtend) tt 6)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'ExtensionLineOffset) tt 7)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'Fit) tt 8)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'HorizontalTextPosition) tt 9)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'Layer) tt 10)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'LinearScaleFactor) tt 11)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'Measurement) tt 12)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'ObjectName) tt 13)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'PrimaryUnitsPrecision) tt 14)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'Rotation) tt 15)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'RoundDistance) tt 16)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'ScaleFactor) tt 17)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'StyleName) tt 18)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'TextColor) tt 19)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'TextFill) tt 20)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'TextHeight) tt 21)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'TextOverride) tt 22)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'TextPrefix) tt 23)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'TextRotation) tt 24)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'TextStyle) tt 25)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'TextSuffix) tt 26)
          (putinexcel-indexed "dimEntities" (vlax-get-property obj 'UnitsFormat) tt 27)
        )
     )
     (setq nn (1+ nn))
  )
  tt
)
;--------------------


(defun chk17-extractions()
   ;----------------chk 17 linetypes
   (dumplinetypes)
)

(defun chk18-extractions ()
   ;--------------- chk 18 purge check
   (if (checkpurge)
      (putinexcel "purge" "ok")
      (putinexcel "purge" "x")
    )
)

;---------- linetype dump
(defun dumplinetypes ()
   (blank-cells "linetype" 2 20)
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  
  (setq linetypes (vlax-get-property doc 'Linetypes)) 
  (setq count (vlax-get-property linetypes 'Count))
  (setq nn 0)
  (setq return 0)
  (setq next 0)
  (while (< nn count)
    (setq obj (vlax-invoke-method linetypes 'Item nn))
    (setq objname (vlax-get-property obj 'Name))
    (setq nn (1+ nn))
    ;(putinexcel-indexed "linetype" objname nn 1)
    (if (not (hasline objname))
       (progn
          (setq next (1+ next))
          (putinexcel-indexed "linetype" objname next 1)
       )
    )
  )
  return
)
(defun hasline (str)
  (wcmatch str "*|*")
)
   
;---------dim dump
(defun dumpdims (/ nn) ; must modify to access all entities
  (blank-cells "dimstyle" 22 20)
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  
  (setq dimstyles (vlax-get-property doc "DimStyles"))
  (setq count (vlax-get-property dimstyles 'Count))
  (setq nn 0)
  (setq return 0)
  (setq next 0)
  (while (< nn count)
    (setq obj (vlax-invoke-method dimstyles 'Item nn))
    (setq objname (vlax-get-property obj 'Name))
    (setq ent (vlax-vla-object->ename obj))
    (setq entlist (entget ent))
    (setq nn (1+ nn))
    (if (not (hasline objname))
       (progn
          (setq next (1+ next))
          (putinexcel-indexed "dimstyle" objname next 1)
          (put-cdim 70 entlist 2);70 - xref 1
          ;(put-cdim 3 entlist 3);3 - dimpost 2
          ;(put-cdim 4 entlist 4);4 - dimapost 3
          (put-cdim 40 entlist 3);40 - dimscale 4 ****** 1.0
          ;(put-cdim 41 entlist 6);41 - dimasz 5
          ;(put-cdim 42 entlist 7);42 - dimexo 6
          ;(put-cdim 43 entlist 8);43 - dimdli 7
          ;(put-cdim 44 entlist 9);44 - dimexe 8
          (put-cdim 45 entlist 4);45 - dimrnd 9 ******** 0.00
          ;(put-cdim 46 entlist 11);46 - dimdle 10
          ;(put-cdim 47 entlist 12);47 - dimtp 11
          ;(put-cdim 48 entlist 13);48 - dimtm 12
          (put-cdim 140 entlist 5);140 - dimtxt 13 ******** 0.09
          ;(put-cdim 141 entlist 15);141 - dimcen 14
          ;(put-cdim 142 entlist 16);142 - dimtsz 15
          ;(put-cdim 143 entlist 17);143 - dimaltf 16
          (put-cdim 144 entlist 6);144 - dimlfac 17 ******* 1.0
          ;(put-cdim 145 entlist 19);145 - dimtvp 18
          ;(put-cdim 146 entlist 20);146 - dimtfac 19
          ;(put-cdim 147 entlist 21);147 - dimgap 20
          ;(put-cdim 148 entlist 22);148 - dimaltrnd 21
          ;(put-cdim 71 entlist 23);71 - dimtol 22 
          ;(put-cdim 72 entlist 24);72 - dimlim 23
          ;(put-cdim 73 entlist 25);73 - dimtih 24
          ;(put-cdim 74 entlist 26);74 - dimtoh 25
          ;(put-cdim 75 entlist 27);75 - dimse1 26
          ;(put-cdim 76 entlist 28);76 - dimse2 27
          (put-cdim 77 entlist 7);77 - dimtad 28
          ;(put-cdim 78 entlist 30);78 - dimzin 29
          ;(put-cdim 79 entlist 31);79 - dimazin 30
          ;(put-cdim 170 entlist 32);170 - dimalt 31
          ;(put-cdim 171 entlist 33);171 - dimaltd 32
          ;(put-cdim 172 entlist 34);172 - dimtofl 33
          ;(put-cdim 173 entlist 35);173 - dimsah 34
          ;(put-cdim 174 entlist 36);174 - dimtix 35
          ;(put-cdim 175 entlist 37);175 - dimsoxd 36
          (put-cdim 176 entlist 8);176 - dimclrd 37
          (put-cdim 177 entlist 9);177 - dimclre 38 ******** 0
          (put-cdim 178 entlist 10);178 - dimclrt 39 ******** 0
          ;(put-cdim 179 entlist 41);179 - dimadec 40
          (put-cdim 271 entlist 11);271 - dimdec 41 ******** 2
          ;(put-cdim 272 entlist 43);272 - dimtdec 42
          ;(put-cdim 273 entlist 44);273 - dimaltu 43
          ;(put-cdim 274 entlist 45);274 - dimalttd 44
          (put-cdim 275 entlist 12);275 - dimaunit 45
          ;(put-cdim 276 entlist 47);276 - dimfrac 46 ********* dimunit equivalent
          ;(put-cdim 277 entlist 48);277 - dimlunit 47 ********* 0,1
          ;(put-cdim 278 entlist 49);278 - dimdsep 48 
          ;(put-cdim 279 entlist 50);279 - dimtmove 49
          ;(put-cdim 280 entlist 51);280 - dimjust 50 
          ;(put-cdim 281 entlist 52);281 - dimsd1 51
          ;(put-cdim 282 entlist 53);282 - dimsd2 52
          ;(put-cdim 283 entlist 54);283 - dimtolj 53
          ;(put-cdim 284 entlist 55);284 - dimtzin 54
          ;(put-cdim 285 entlist 56);285 - dimaltz 55 
          ;(put-cdim 286 entlist 57);286 - dimalttz 56
          (put-cdim 287 entlist 13)
          ;(put-cdim 288 entlist 58);288 - dimupt 57
          (put-cdim 289 entlist 14);289 - dimatfit 58
          ;(put-cdim 340 entlist 60);340 - dimtxsty 59 ****** access- must be Standard
          ;(put-cdim 341 entlist 61);341 - dimldrblk 60
          ;(put-cdim 342 entlist 62);342 - dimblk 61
          ;(put-cdim 343 entlist 63);343 - dimblk1 62
          ;(put-cdim 344 entlist 64);344 - dimblk2 63
          (put-cdim 371 entlist 15);371 - dimlwd 64
          (put-cdim 372 entlist 16);372 - dimlwe 65
       )
    )
  )
  return
)

(defun put-cdim (cc entlist clm)
   (setq cx (cdr (assoc cc entlist)))
   (putinexcel-indexed "dimstyle" cx next clm)
)


;--------block dump

(defun dumpblocks (/ nn)
  (blank-cells "blockname" 2 150)
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  
  (setq blocks (vlax-get-property doc "Blocks")) 
  (setq count (vlax-get-property blocks 'Count))
  (setq nn 0)
  (setq return 0)
  (setq next 0)
  (while (< nn count)
    (setq obj (vlax-invoke-method blocks 'Item nn))
    (setq objname (vlax-get-property obj 'Name))
    (setq nn (1+ nn))
    (if (and
          (not (hasline objname))
          (/= objname "*MODEL_SPACE")
          (/= objname "*PAPER_SPACE")
        )
       (progn
          (setq next (1+ next))
          (putinexcel-indexed "blockname" objname next 1)
          
          ;(putinexcel-indexed (vlax-get-property obj 'BlockScaling) objname next 2)
          ;;(putinexcel-indexed (vlax-get-property obj 'Count) objname next 2)
          ;(putinexcel-indexed (vlax-get-property obj 'IsXRef) objname next 4)
          ;(putinexcel-indexed (vlax-get-property obj 'Origin) objname next 5)
          ;(putinexcel-indexed (vlax-get-property obj 'Units) objname next 6)

          

       )
    )
  )
  return
)
(defun hasline (str)
  (wcmatch str "*|*")
)

;---------styles dump
(defun dumpstyles (/ nn)
; note that the font style is an extension to the font file name - eg ARIAL and ARIALBD
  (blank-cells "textstyle" 5 20)
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  
  (setq styles (vlax-get-property doc "TextStyles")) 
  (setq count (vlax-get-property styles 'Count))
  (setq nn 0)
  (setq return 0)
  (setq next 0)
  (while (< nn count)
    (setq obj (vlax-invoke-method styles 'Item nn))
    (setq objname (vlax-get-property obj 'Name))
    (setq bff (vlax-get-property obj 'BigFontFile))
    (setq ff (vlax-get-property obj 'FontFile))
    (setq txht (vlax-get-property obj 'Height))
    (setq txwd (vlax-get-property obj 'Width))
    (setq nn (1+ nn))

    (if (not (hasline objname))
       (progn
          (setq next (1+ next))
          (putinexcel-indexed "textstyle" objname next 1)
          (putinexcel-indexed "textstyle" bff next 2)
          (putinexcel-indexed "textstyle" ff next 3)
          (putinexcel-indexed "textstyle" txht next 4)
          (putinexcel-indexed "textstyle" txwd next 5)
          
       )
    )
  )
  return
)
(defun hasline (str)
  (wcmatch str "*|*")
)
;-------------text dump
(defun dumptext()
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  (setq tt 0)
  
  (setq tt (textspacedump doc "*MODEL_SPACE" tt))
  (textspacedump doc "*PAPER_SPACE" tt)
)

(defun textspacedump (doc curspace tt)
  (setq blocks (vlax-get-property doc "Blocks"))
  (setq modelspace (vlax-invoke-method blocks 'Item curspace))
  (setq count (vlax-get-property modelspace 'Count))
  (setq nn 0)
  (while (< nn count)
     (setq obj (vlax-invoke-method modelspace 'Item nn))
     (if (= "AcDbText" (vlax-get-property obj 'ObjectName))
        (progn
           (setq tt (1+ tt))
           (setq txht (vlax-get-property obj 'Height))
           (setq txlyr (vlax-get-property obj 'Layer))
           (setq txstr (vlax-get-property obj 'TextString))
           (setq txstl (vlax-get-property obj 'StyleName))
           (putinexcel-indexed "textEntity" txht tt 1)
           (putinexcel-indexed "textEntity" txlyr tt 2)
           (putinexcel-indexed "textEntity" txstr tt 3)
           (putinexcel-indexed "textEntity" txstl tt 4)
           (putinexcel-indexed "textEntity" curspace tt 5)
        )
     )
     (setq nn (1+ nn))
  )
  tt
)
  

;---------layer dump
(defun chk9-extractions (/ nn)
  (blank-cells "layers" 2 150)
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  
  (setq layers (vlax-get-property doc "Layers")) 
  (setq count (vlax-get-property layers 'Count))
  (setq nn 0)
  (setq return 0)
  (setq next 0)
  (while (< nn count)
    (setq obj (vlax-invoke-method layers 'Item nn))
    (setq objname (vlax-get-property obj 'Name))
    (setq nn (1+ nn))
   ; (putinexcel-indexed "layers" objname nn 1)
    (if (not (hasline objname))
       (progn
          (setq next (1+ next))
          (putinexcel-indexed "layers" objname next 1)
       )
    )
  )
  return
)
(defun hasline (str)
  (wcmatch str "*|*")
)

;---------xref export lisps
(defun listxrefs (/ nn count acadobj docs dwgname database return mm)
  (blank-cells "XrefList" 14 40) ;  clears previous xrefs from template

	; ModelSpace PaperSpace
  (setq database (get-all-blocks "Blocks"))
  (setq count (vlax-get-property database 'Count))
  (setq nn 0)
  (setq next 0)
  
  (while (< nn count) ; cycle through all blocks
    (setq obj (vlax-invoke-method database 'Item nn)) ; get object nn in database
    ;(setq objtype (vlax-get-property obj 'ObjectName))
    (setq xname (vlax-get-property obj 'Name))        ; get name of this object
    (if	;(and
	    (/= :vlax-false (vlax-get-property obj 'IsXref))            ; if object is an xref
	     ;(/= "*" (substr xname 1 1))
	     ;)
      (progn
	      (setq ent (vlax-vla-object->ename obj))       ; convert to ent from obj
	      (if (setq entlist (entget ent))
	        (progn
	          (setq c360 (cdr (assoc 360 entlist)))
            (setq tobj (vlax-ename->vla-object c360))
            (setq tbl (entget c360))
            (setq c70 nil)
            (setq c70 (cdr (assoc 70 tbl)))
          )
        )
        (if (and c70 (/= c70 0))
          (progn
            (setq next (1+ next))
            (putinexcel-indexed "XrefList" xname next 1)
            (if	(/= (logand 2 c70) 2)
              (progn
                (setq bpath (vlax-get-property obj 'Path))
                (setq borg (vlax-safearray->list       ; get coords
                      (vlax-variant-value
                        (vlax-get-property obj 'Origin)
                      )
                    )
                )
                (setq eorg (strcat (rtos (nth 0 borg)) ; turn coords to string
                    ","
                    (rtos (nth 1 borg))
                    ","
                    (rtos (nth 2 borg))
                  )
                )
                (putinexcel-indexed "XrefList" bpath next 2)
                (if (hasslash bpath)
                  (putinexcel-indexed "XrefList" "Yes" next 13)
                  (putinexcel-indexed "XrefList" "No" next 13)
                )
                (putinexcel-indexed "XrefList" eorg next 11)
                (putinexcel-indexed "XrefList" c70 next 3)
                (if (= (logand 1 c70) 1)
                  (putinexcel-indexed "XrefList" "Yes" next 4)
                )
                (if (= (logand 2 c70) 2)
                  (putinexcel-indexed "XrefList" "Yes" next 5)
                )
                (if (= (logand 4 c70) 4)
                  (putinexcel-indexed "XrefList" "Yes" next 6)
                )
                (if (= (logand 8 c70) 8)
                  (putinexcel-indexed "XrefList" "Yes" next 7)
                )
                (if (= (logand 16 c70) 16)
                  (putinexcel-indexed "XrefList" "Yes" next 8)
                )
                (if (= (logand 32 c70) 32)
                  (putinexcel-indexed "XrefList" "Yes" next 9)
                )
                (if (= (logand 64 c70) 64)
                  (putinexcel-indexed "XrefList" "Yes" next 10)
                )
              )
            )
          )
        )
      )

    )
    (setq nn (1+ nn))
  )
)

(defun blank-cells (fromrange inx iny / nn mm) ; clears cells 40 height by 20 width
  (setq nn 1)
  (while (< nn iny)			; blank cells
    (setq mm 1)
    (while (< mm inx)
      (putinexcel-indexed fromrange "" nn mm)
      (setq mm (1+ mm))
    )
    (setq nn (1+ nn))
  )
)
(defun get-all-blocks (dtype / acadobj docs dwgname database) ; dtype could be blocks table, model or paper space
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq	dwgname
	 (vl-registry-read
	   "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker"
	   "Current drawing"
	 )
  )
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  (setq database (vlax-get-property doc dtype))
)
(defun hasslash (str)
  (wcmatch str "*\\*")
)

;-------- check if document can be purged
(defun checkpurge ()
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
 ; (vlax-invoke-method doc "PurgeAll")
 ; (setq return (vlax-get-property doc "Saved"))

  (setq blocks (vlax-get-property doc 'Blocks))
  (setq nn 2)
  (setq notmissing T)
  (while (and (< nn (vlax-get-property blocks 'Count)) notmissing)
     (setq notmissing (checkforblock (vlax-invoke-method blocks 'Item nn)))
     (setq nn (1+ nn))
  )
  
  notmissing
)

(defun checkforblock (blk / nn mm)
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  
  (setq blkname (vlax-get-property blk 'Name))
  (setq blocks (vlax-get-property doc 'Blocks))
  (setq nn 0)
  (setq found nil)
  (while (and (< nn (vlax-get-property blocks 'Count)) (not found))
    (setq block (vlax-invoke-method blocks 'Item nn))
    (setq mm 0)
    (while (and (< mm (vlax-get-property block 'Count)) (not found))
      (setq obj (vlax-invoke-method block 'Item mm))
      (setq objname (vlax-get-property obj 'ObjectName))
      (if (= objname "AcDbBlockReference")
        (if (= (vlax-get-property obj 'Name) blkname)
          (setq found T)
        )
      )
      (setq mm (1+ mm))
    )
    (setq nn (1+ nn))
  )
  found
)

;-------- get all entities of type objtype on space

(defun objsum (objtype space / nn)
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  (setq database (vlax-get-property doc space)) ; ModelSpace PaperSpace
  (setq count (vlax-get-property database 'Count))
  (setq nn 0)
  (setq return 0)
  (while (< nn count)
    (setq obj (vlax-invoke-method database 'Item nn))
    (setq objname (vlax-get-property obj 'ObjectName))
    (if (or (= objname objtype) (= objtype "all"))
      (setq return (1+ return))
    )
    (setq nn (1+ nn))
  )
  return
)

;------- get all entities on layer 0 - by space need to check both spaces
(defun layer0sum (space / nn)
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  (setq database (vlax-get-property doc space)) ; ModelSpace PaperSpace
  (setq count (vlax-get-property database 'Count))
  (setq nn 0)
  (setq return 0)
  (while (< nn count)
    (setq obj (vlax-invoke-method database 'Item nn))
    (setq lyrname (vlax-get-property obj 'Layer))
    (setq objname (vlax-get-property obj 'ObjectName))
    (if (and (= lyrname "0") (/= objname "AcDbViewport")) ; ignore viewport on layer 0
      (setq return (1+ return))
    )
    (setq nn (1+ nn))
  )
  return
)

;------- get all attribute in a block
(defun getatts (objectid space /  nn)
    (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  (setq database (vlax-get-property doc space)) ; ModelSpace PaperSpace
  (setq count (vlax-get-property database 'Count))
  (setq nn 0)
  (setq return '())
  (while (< nn count)
    (setq obj (vlax-invoke-method database 'Item nn))
    (setq objid (vlax-get-property obj 'ObjectID))
    (if (= objid objectid)
       (progn
	 (setq ent (vlax-vla-object->ename obj))
	 (setq ent (entnext ent))
	 (setq entlist (entget ent))
	 (setq c0 (cdr (assoc 0 entlist)))
	 (while (/= c0 "SEQEND")
	   (setq atttag (cdr (assoc 2 entlist)))
	   (setq atttext (cdr (assoc 1 entlist)))
           (setq return (append return (list (list atttag atttext))))
	   (setq ent (entnext ent))
	   (setq entlist (entget ent))
	   (setq c0 (cdr (assoc 0 entlist)))
	 )
       )
    )
    (setq nn (1+ nn))
  )
  return
)

;------- get single block by blkname and space
(defun getblk (blkname space /  nn)
    (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  (setq database (vlax-get-property doc space)) ; ModelSpace PaperSpace
  (setq count (vlax-get-property database 'Count))
  (setq nn 0)
  (setq return 0)
  (while (< nn count)
    (setq obj (vlax-invoke-method database 'Item nn))
    (setq objtype (vlax-get-property obj 'ObjectName))
    (if (= objtype "AcDbBlockReference")
      (if (= (vlax-get-property obj 'Name) blkname)
	(setq return obj)
      )
    )
    (setq nn (1+ nn))
  )
  return
)


; ------ block count function - send space and block name and will return number of blocks -
(defun blkcount (blkname space / nn)
  (setq acadobj (vlax-get-acad-object))
  (setq docs (vlax-get-property acadobj 'Documents))
  (setq dwgname (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Current drawing"))
  (setq doc (vlax-invoke-method docs 'Item dwgname))
  (setq database (vlax-get-property doc space)) ; ModelSpace PaperSpace
  (setq count (vlax-get-property database 'Count))
  (setq nn 0)
  (setq return 0)
  (while (< nn count)
    (setq obj (vlax-invoke-method database 'Item nn))
    (setq objtype (vlax-get-property obj 'ObjectName))
    (if (= objtype "AcDbBlockReference")
      (if (= (vlax-get-property obj 'Name) blkname)
	(setq return (1+ return))
      )
    )
    (setq nn (1+ nn))
  )
  return
)