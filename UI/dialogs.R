showReadonlyDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogReadonly$title,
    lang$nav$dialogReadonly$desc,
    footer = tagList(
      modalButton(lang$nav$dialogReadonly$cancelButton),
      actionButton("btSaveReadonly", label = lang$nav$dialogReadonly$okButton, class = "btOrange")),
    fade = TRUE, easyClose = FALSE
  ))
}

showNewScenDialog <- function(tmpScenName){
  showModal(modalDialog(
    title = lang$nav$dialogNewScen$title,
    textInput("scenName", lang$nav$dialogNewScen$desc,
              value = tmpScenName),
    shinyjs::hidden(
      tags$div(id = "bad.scen.name", class = "errMsg", lang$nav$dialogNewScen$badName),
      tags$div(id = "scen.exits", class = "errMsg", lang$nav$dialogNewScen$scenExits)
    ),
    footer = tagList(
      tags$div(id = "dialogSaveInit",
               modalButton(lang$nav$dialogNewScen$cancelButton),
               actionButton("btCheckName", lang$nav$dialogNewScen$okButton, class = "btOrange")
      ),
      shinyjs::hidden(
        tags$div(id = "dialogSaveConfirm",
                 actionButton("btNewName", lang$nav$dialogNewScen$btNewName),
                 actionButton("btSaveConfirm", lang$nav$dialogNewScen$btOverride, class = "btOrange")
        )
      )
    ),
    fade = TRUE, easyClose = FALSE))
}

showOverrideScenDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogLoadScen$titleOverrideInput,
    lang$nav$dialogLoadScen$descOverrideInput,
    footer = tagList(
      modalButton(lang$nav$dialogLoadScen$cancelButton),
      actionButton("btOverrideScen", label = lang$nav$dialogLoadScen$okButton, class = "btOrange")),
    fade = FALSE, easyClose = FALSE))
}

showCloseScenDialog <- function(scenId){
  showModal(modalDialog(
    title = lang$nav$dialogCloseScen$title,
    lang$nav$dialogCloseScen$desc,
    footer = tagList(
      modalButton(lang$nav$dialogCloseScen$cancelButton),
      actionButton("btCloseFinal_" %+% scenId, lang$nav$dialogCloseScen$okButton, class = "btOrange")),
    fade=FALSE, easyClose=FALSE))
}

showRemoveDeletedScenFromUIDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogDeleteScen$removeFromUI$title,
    lang$nav$dialogDeleteScen$removeFromUI$desc,
    footer = tagList(
      modalButton(lang$nav$dialogDeleteScen$removeFromUI$cancelButton),
      actionButton("btRemoveDeletedConfirm", label = lang$nav$dialogDeleteScen$removeFromUI$okButton, class = "btOrange")),
    fade=TRUE, easyClose=FALSE))
}

showRemoveActiveScenFromUIDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogRemoveScen$title,
    lang$nav$dialogRemoveScen$desc,
    footer = tagList(
      modalButton(lang$nav$dialogRemoveScen$cancelButton),
      actionButton("btRemoveConfirm", label = lang$nav$dialogRemoveScen$okButton, class = "btOrange")),
    fade=TRUE, easyClose=FALSE))
}

showDeleteScenDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogDeleteScen$title,
    lang$nav$dialogDeleteScen$desc,
    footer = tagList(
      modalButton(lang$nav$dialogDeleteScen$cancelButton),
      actionButton("btDeleteConfirm", lang$nav$dialogDeleteScen$okButton, class = "btOrange")),
    fade=TRUE, easyClose=FALSE))
}

showRemoveExistingOutputDataDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogExistingOutput$title,
    lang$nav$dialogExistingOutput$desc,
    footer = tagList(
      modalButton(lang$nav$dialogExistingOutput$cancelButton),
      actionButton("btSaveOutput", label = lang$nav$dialogExistingOutput$saveOutputButton),
      actionButton("btRemoveOutput", label = lang$nav$dialogExistingOutput$discardOutputButton, class = "btOrange")),
    fade = TRUE, easyClose = FALSE))
}