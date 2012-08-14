(vl-load-com)
(setq instdirINTEL (vl-registry-read "HKEY_CURRENT_USER\\Software\\ACT\\Intel-Standards-Checker" "Install directory"))
(if (setq runonce (findfile (strcat instdirINTEL "\\runonce.lsp")))
   (progn
      (load runonce)
      (setq delcmd (strcat "erase " runonce))
      (command "shell" delcmd)
   )
)




