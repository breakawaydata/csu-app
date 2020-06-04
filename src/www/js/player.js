$( document ).ready(function() {
  $(document).on('click','.main .player-card', function(){
    
    Shiny.setInputValue('main-player', $(this).attr("id"));
    $('.ui.breadcrumb').find('.section')[2].click();
    window.scrollTo(0, 0);
  });
});
