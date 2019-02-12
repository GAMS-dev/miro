// counter
count = 1;
// maximum number of scenarios that can be loaded in compare view
maxNumScen = 50;
$(document).keyup(function(event) {
  
  // ENTER will confirm modal dialogues
  if(event.keyCode == 13 && !event.ctrlKey){
    if($("#shiny-modal").find(".selectize-input.input-active").length > 0 || $("#shiny-modal").find("*[data-dismiss='modal']").is(":focus")){
      return;
    }
    $(".bt-gms-confirm:visible:enabled").click();
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
  if(event.ctrlKey && event.altKey && event.keyCode === 83) {
    Shiny.setInputValue("btSave", 1, {priority: "event"});
  }
  // Solve shortcut: CTRL + ALT + ENTER
  if(event.ctrlKey && event.altKey && event.keyCode === 13 && $("#btSolve").is(":enabled") && $("#btSolve").is(":visible")) {
    $("#btSolve").click();
  }
  // Remove shortcut: CTRL + ALT + R
  if(event.ctrlKey && event.altKey && event.keyCode === 82) {
    Shiny.setInputValue("btDelete", 1, {priority: "event"});
  }
  // Close shortcut (remove button in input sheet): CTRL + ALT + C
  if(event.ctrlKey && event.altKey && event.keyCode === 67 && $(".btRemove").is(":visible")) {
    $(".btRemove:visible").click();
  }
  // Close shortcut (remove button in output sheet): CTRL + ALT + C
  if(event.ctrlKey && event.altKey && event.keyCode === 67) {
    for (i = 2; i <= maxNumScen; i++) { 
      id = '#close_' + i;
      if($(id).is(":visible")){
        $(id).click();
      }
    }
  }
  // Fullscreen mode (hide sidebar) shortcut: CTRL + ALT + F
  if(event.ctrlKey && event.altKey && event.keyCode === 70) {
    $("body").toggleClass("sidebar-collapse");
  }
  // Select input menu shortcut: CTRL + ALT + 1
  if(event.ctrlKey && event.altKey && event.keyCode === 49) {
    $("a[href='#shiny-tab-inputData']").click();
  }
  // Select output menu shortcut: CTRL + ALT + 2
  if(event.ctrlKey && event.altKey && event.keyCode === 50) {
    tab = $("a[href='#shiny-tab-outputData']");
    if(tab.length > 0){
      tab.click();
    }else{
      $("a[href='#shiny-tab-importData']").click();
    }
  }
  // Select gams interaction menu shortcut: CTRL + ALT + 3
  if(event.ctrlKey && event.altKey && event.keyCode === 51) {
    tab = $("a[href='#shiny-tab-gamsinter']");
    if(tab.length > 0){
      tab.click();
    }else{
      $("a[href='#shiny-tab-loadResults']").click();
    }
  }
  // Select scenario menu shortcut: CTRL + ALT + 4
  if(event.ctrlKey && event.altKey && event.keyCode === 52) {
    tab = $("a[href='#shiny-tab-scenarios']").click();
  }
  if(event.ctrlKey && event.altKey && event.keyCode === 53) {
    tab = $("a[href='#shiny-tab-hcubeAnalyze']");
    if(tab.length > 0){
      tab.click();
    }
  }
  // Table view (scenario compare mode) shortcut: CTRL + ALT + T
  if(event.ctrlKey && event.altKey && event.keyCode === 84) {
    id = '#btGraphIn';
    if($(id).is(":visible") && $(id).is(":enabled")){
      $(id).click();
    }
    id = '#outputTableView';
    if($(id).is(":visible")){
      $(id).click();
    }
    for (i = 2; i <= maxNumScen + 3; i++) { 
      id = '#table_' + i;
      if($(id).is(":visible")){
        $(id).click();
      }
    }
  }
  // Select next tab shortcut: CTRL + ALT + arrow right
  if(event.ctrlKey && event.altKey && event.keyCode === 39) {
    Shiny.onInputChange("tabsetShortcutNext", count);
    count++;
  }
  // Select previous tab shortcut: CTRL + ALT + arrow left
  if(event.ctrlKey && event.altKey && event.keyCode === 37) {
    Shiny.onInputChange("tabsetShortcutPrev", count);
    count++;
  }
  // Nest to next lower tabset shortcut: CTRL + ALT + arrow down
  if(event.ctrlKey && event.altKey && event.keyCode === 40) {
    Shiny.onInputChange("tabsetShortcutNest", count);
    count++;
  }
  // Unnest to next higher tabset shortcut: CTRL + ALT + arrow up
  if(event.ctrlKey && event.altKey && event.keyCode === 38) {
    Shiny.onInputChange("tabsetShortcutUnnest", count);
    count++;
  }
  // Activate/deactivate scenario comparison mode: CTRL + ALT + space
  if(event.ctrlKey && event.altKey && event.keyCode === 32 && $("#btCompareScen").is(":enabled") && $("#btCompareScen").is(":visible")) {
    $("#btCompareScen").click();
  }
});