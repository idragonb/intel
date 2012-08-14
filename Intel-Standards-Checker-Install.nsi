!define VERSION "2.0.1.9"
Name "Intel-Standards-Checker ${VERSION}"
OutFile "Intel-Standards-Checker-Install.exe - ${VERSION}.exe"

VIProductVersion "2.0.1.9"
VIAddVersionKey ProductName "Intel-Standards-Checker ${VERSION}"
VIAddVersionKey Comments "Intel Standards Checker - Compliance for: FAB 28 - 2005 Rev.8 Version 3"
VIAddVersionKey CompanyName "Applying Computer Technologies"
VIAddVersionKey LegalCopyright "Applying Computer Technologies"
VIAddVersionKey FileDescription "Installs Intel-Standards-Checker ${VERSION}"
VIAddVersionKey ProductVersion ${VERSION}



Name Intel-Standards-Checker
InstallDir "C:\ACT\Intel-Standards-Checker"


LicenseData install\License.txt

Page license
Page directory
Page instfiles

Section RegistryWork
  WriteRegStr HKCU "Software\ACT\Intel-Standards-Checker" "Install directory" "$INSTDIR"

SectionEnd

Section Copyfiles
  SetOutPath $INSTDIR
  File Intel-Standards-Checker.odcl
  File OpenDCL.Runtime.5.1.2.3.msi
  File Prototype.xlsx
  File install\install.dwg
  File install\acaddoc.lsp
  File Intel-Standards-Checker.cui
  File Intel-Standards-Checker.mnl
  File "Intel Standards Checker.lsp"
  File "install\runonce.lsp"
  File "excel utilities.lsp"
  File "extract-all.lsp"
  File "install\prototype.dwg"
  File "install\revision history.txt"
SectionEnd

Section InstallExtras
   ExecShell "open" "$INSTDIR\install.dwg"
   ExecShell "open" '"$INSTDIR\OpenDCL.Runtime.5.1.2.3.msi"'
   BringToFront
SectionEnd

