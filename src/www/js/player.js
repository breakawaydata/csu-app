$( document ).ready(function() {
  $(document).on('click','.' + consts.dom.player_card_class, function(){
    
    Shiny.setInputValue('main-player', $(this).attr("id"));
    Shiny.setInputValue('menu-level', 'player');
    $('#' + consts.dom.menu_navigation_id).find('.section')[0].classList.remove('active');
    changeView('player');
    window.scrollTo(0, 0);
  });
});
