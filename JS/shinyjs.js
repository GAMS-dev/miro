shinyjs.scrollDown = function(params){
  setTimeout(function(){
    var elem = document.getElementById(params);
    elem.scrollTop = elem.scrollHeight;
  }, 100);
};