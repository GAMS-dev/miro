let indices            = [];
let indexAliases       = [];
let scalarIndices      = [];
let scalarIndexAliases = [];

function addSelectInput(id){
  optionsHTML = '';
  let count = $('#chart_ydata_container .form-group').length;
  id = id + count;
  for (let i = 0, itemLen = scalarIndices.length; i < itemLen; 
  optionsHTML += '<option value="' + scalarIndices[i] + '"' + (i===1? ' selected' : '') + 
  '>' + scalarIndexAliases[i++] + '</option>\n');
  
  $('#chart_ydata_container').append('<div class="form-group">\n<label class="control-label" for="' +
  id + '">What should be plotted on the y axis?</label><div><select id="' +
  id + '">' + optionsHTML + '</select>\n</div>\n</div><hr>');
	$('#' + id).selectize({
	  onChange: function(value) {
	    Shiny.setInputValue('chart_ydata', [count, value]);
    }
	});
}

$(document).ready(function () {
  Shiny.addCustomMessageHandler('gms-setIndices', function (indicesFromR) {
    $.each(indicesFromR, function(key, val) {
      if(!$.isArray(val)){
        indicesFromR[key] = [val];
      }
    });
    console.log(indicesFromR);
    indices = indicesFromR.indices;
    indexAliases = indicesFromR.aliases;
    scalarIndices = indicesFromR.scalarIndices;
    scalarIndexAliases = indicesFromR.scalarAliases;
  });
});