(load "excel utilities.lsp")

(defun chk3 (/ retval)
  (setq	dwgname
	 (vl-registry-read
	   "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker"
	   "Current drawing"
	 )
  )
  (putinexcel "thisdwg" dwgname)
  (listxrefs)
  (setq result (getresult "chk3"))
  (if (= result "x")
    (copytab "chk3results" "chk3")
  )
)

(defun listxrefs (/ nn count acadobj docs dwgname database return mm)
  (blank-cells) ;  clears previous xrefs from template

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



(defun hasslash (str)
  (wcmatch str "*\\*")
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

(defun blank-cells (/ nn mm) ; clears cells 40 height by 20 width
  (setq nn 1)
  (while (< nn 40)			; blank cells
    (setq mm 1)
    (while (< mm 20)
      (putinexcel-indexed "XrefList" "" nn mm)
      (setq mm (1+ mm))
    )
    (setq nn (1+ nn))
  )
)