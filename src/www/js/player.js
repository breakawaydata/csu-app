$( document ).ready(function() {
  $(document).on('click','.' + consts.dom.player_card_class, function(){
    
    try_ga('click', 'body', 'player', $(this).attr("id"));
    
    $('#' + consts.dom.logo_card_id + ' div.header')[0]
      .innerHTML = $(this).attr("data-name");
    $('#' + consts.dom.logo_card_id + ' div.image')
      .css('background-image', 'url("' + $(this).attr("data-picture") + '")');
    $('#' + consts.dom.logo_card_id + ' div.meta > span')[0]
      .innerHTML = $(this).attr("data-position");
    
    Shiny.setInputValue('main-player', $(this).attr("id"));
    Shiny.setInputValue('menu-level', 'player');
    $('#' + consts.dom.menu_navigation_id).find('.section')[0].classList.remove('active');
    changeView('player');
    window.scrollTo(0, 0);
  });
});
