$(window).on('beforeunload', function() {
    return "Are you sure you want to leave? Unsaved changes will be lost!";
});

// counter
count = 1;
// maximum number of scenarios that can be loaded in compare view
maxNumScen = 50;
$(document).keyup(function(event) {
  
  // ENTER will confirm modal dialogues
  if(event.keyCode == 13 && !event.ctrlKey){
    if ($("#btLoadLocal").is(":visible") && $("#btLoadLocal").is(":enabled")) {
        $("#btLoadLocal").click();
    }
    if ($("#btLoadScenConfirm").is(":visible")) {
        $("#btLoadScenConfirm").click();
    }
    if ($("#btOverrideScen").is(":visible")) {
        $("#btOverrideScen").click();
    }
    if ($("#btRemoveOutput").is(":visible")) {
        $("#btRemoveOutput").click();
    }
    if ($("#btCheckName").is(":visible")) {
        $("#btCheckName").click();
    }
    if ($("#btSaveConfirm").is(":visible")) {
        $("#btSaveConfirm").click();
    }
    if ($("#btRemoveConfirm").is(":visible")) {
        $("#btRemoveConfirm").click();
    }
    if ($("#btOverrideLocal").is(":visible")) {
        $("#btOverrideLocal").click();
    }
    if ($("#btDeleteConfirm").is(":visible")) {
        $("#btDeleteConfirm").click();
    }
    for (i = 2; i <= maxNumScen; i++){ 
      id = '#close_final_' + i;
      if($(id).is(":visible")){
        $(id).click();
      }
    }
  }
  // ESC will close modal dialogues
  if(event.keyCode === 27) {
    $('.modal').modal('hide');
  }
  // Import shortcut: CTRL + ALT + I
  if(event.ctrlKey && event.altKey && event.keyCode === 73) {
    if($("#btImport").is(":visible")){
      $("#btImport").click();
    }else if($("#btLoadScen").is(":visible")){
      $("#btLoadScen").click();
    }
  }
  // SAVE shortcut: CTRL + ALT + S
  if(event.ctrlKey && event.altKey && event.keyCode === 83 && $("#btSave").is(":visible")) {
    if($("#btSave").is(":enabled")){
      $("#btSave").click();
    }else if($("#btSaveAs").is(":enabled")){
      $("#btSaveAs").click();
    }
  }
  // Solve shortcut: CTRL + ALT + ENTER
  if(event.ctrlKey && event.altKey && event.keyCode === 13 && $("#btSolve").is(":enabled") && $("#btSolve").is(":visible")) {
    $("#btSolve").click();
  }
  // Remove shortcut: CTRL + ALT + R
  if(event.ctrlKey && event.altKey && event.keyCode === 82 && $("#btDelete").is(":enabled") && $("#btDelete").is(":visible")) {
    $("#btDelete").click();
  }
  // Close shortcut (remove button in input sheet): CTRL + ALT + C
  if(event.ctrlKey && event.altKey && event.keyCode === 67 && $("#btRemove").is(":enabled") && $("#btRemove").is(":visible")) {
    $("#btRemove").click();
  }
  // Close shortcut (remove button in output sheet): CTRL + ALT + C
  if(event.ctrlKey && event.altKey && event.keyCode === 67) {
    if($("#btRemoveO").is(":visible")){
      if($("#btRemoveO").is(":enabled")){
        $("#btRemoveO").click();
      }
    }else{
      for (i = 2; i <= maxNumScen; i++) { 
        id = '#close_' + i;
        if($(id).is(":visible")){
          $(id).click();
        }
      }
    }
  }
  // Fullscreen mode (hide sidebar) shortcut: CTRL + ALT + F
  if(event.ctrlKey && event.altKey && event.keyCode === 70) {
    $("body").toggleClass("sidebar-collapse");
  }
  // Select input menu shortcut: CTRL + ALT + 1
  if(event.ctrlKey && event.altKey && event.keyCode === 49) {
    Shiny.onInputChange("sidebar.menu.shortcut", "inputData");
  }
  // Select output menu shortcut: CTRL + ALT + 2
  if(event.ctrlKey && event.altKey && event.keyCode === 50) {
    Shiny.onInputChange("sidebar.menu.shortcut", "outputData");
  }
  // Select gams interaction menu shortcut: CTRL + ALT + 3
  if(event.ctrlKey && event.altKey && event.keyCode === 51) {
    Shiny.onInputChange("sidebar.menu.shortcut", "gamsinter");
  }
  // Select scenario compare menu shortcut: CTRL + ALT + 4
  if(event.ctrlKey && event.altKey && event.keyCode === 52) {
    Shiny.onInputChange("sidebar.menu.shortcut", "scenarios");
  }
  // Select advanced options menu shortcut: CTRL + ALT + 4
  if(event.ctrlKey && event.altKey && event.keyCode === 53) {
    Shiny.onInputChange("sidebar.menu.shortcut", "advanced");
  }
  // Table view (scenario compare mode) shortcut: CTRL + ALT + T
  if(event.ctrlKey && event.altKey && event.keyCode === 84) {
    for (i = 2; i <= maxNumScen + 3; i++) { 
      id = '#table_' + i;
      if($(id).is(":visible")){
        $(id).click();
      }
    }
  }
  // Table view (scenario compare mode) shortcut: CTRL + ALT + T
  if(event.ctrlKey && event.altKey && event.keyCode === 84) {
    for (i = 1; i <= maxNumScen + 3; i++) { 
      id = '#table_' + i;
      if($(id).is(":visible")){
        $(id).click();
      }
      id = '#btGraphIn' + i;
      if($(id).is(":visible")){
        $(id).click();
      }
    }
    id = '#outputTableView';
    if($(id).is(":visible")){
      $(id).click();
    }
  }
  // Select next tab shortcut: CTRL + ALT + arrow right
  if(event.ctrlKey && event.altKey && event.keyCode === 39) {
    Shiny.onInputChange("tabset.shortcut.next", count);
    count++;
  }
  // Select next tab shortcut: CTRL + ALT + arrow left
  if(event.ctrlKey && event.altKey && event.keyCode === 37) {
    Shiny.onInputChange("tabset.shortcut.prev", count);
    count++;
  }
  // Nest to next lower tabset shortcut: CTRL + ALT + arrow down
  if(event.ctrlKey && event.altKey && event.keyCode === 40) {
    Shiny.onInputChange("tabset.shortcut.nest", count);
    count++;
  }
  // Unnest to next higher tabset shortcut: CTRL + ALT + arrow up
  if(event.ctrlKey && event.altKey && event.keyCode === 38) {
    Shiny.onInputChange("tabset.shortcut.unnest", count);
    count++;
  }
  // Activate/deactivate scenario comparison mode: CTRL + ALT + space
  if(event.ctrlKey && event.altKey && event.keyCode === 32 && $("#btCompareScen").is(":enabled") && $("#btCompareScen").is(":visible")) {
    $("#btCompareScen").click();
  }
});