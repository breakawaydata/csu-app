$( document ).ready(function() {
  $(document).on('click','.main .player-card', function(){
    
    Shiny.setInputValue('main-player', $(this).attr("id"));
    // click player section
    $(".levels > div > a")[2].click();
  });
});
