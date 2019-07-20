$( document ).on("shiny:sessioninitialized", function(event) {
  Shiny.setInputValue("clear_rectangle", "FALSE");           
});

function changeDeleteButton() {
  // If leaflet delete button is present, replace it with custom button
  if ( $( "a.leaflet-draw-edit-remove" ).length ) {
    $( "a.leaflet-draw-edit-remove" ).replaceWith( "<button class='delete-button' title='Clear Rectangle'> </button>" );
  }
}

$(function(){
  // Repeat function call every second
  setInterval(changeDeleteButton, 1000);
});


$(document).on('click tap', '.delete-button', function () {
  // When delete button is clicked, change input$clear_rectangle to TRUE
  Shiny.setInputValue("clear_rectangle", "TRUE");
});



Shiny.addCustomMessageHandler('resetInput', function(variableName) {
  // Set input variable to NULL
  Shiny.setInputValue(variableName, null);
});