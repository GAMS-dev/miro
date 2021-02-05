!macro customUnInstall
  MessageBox MB_YESNO "Do you also want to permanently delete the MIRO database with all your settings?" \
    /SD IDNO IDNO Skipped IDYES Accepted

  Accepted:
    RMDir /r "$PROFILE\.miro"
    Goto done
  Skipped:
    Goto done
  done:
!macroend
