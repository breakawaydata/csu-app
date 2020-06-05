$( document ).ready(function() {
  $(document).on('click','.' + consts.dom.player_card_class, function(){
    
    Shiny.setInputValue('main-player', $(this).attr("id"));
    $('#' + consts.dom.menu_navigation_id).find('.section')[2].click();
    window.scrollTo(0, 0);
  });
});
