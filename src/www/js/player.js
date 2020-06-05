$( document ).ready(function() {
  $(document).on('click','.' + consts.dom.player_card_class, function(){
    
    $('#logocard > div > div.content > div.header')[0].innerHTML = $(this).attr("data-name");
    $('#logocard > div > div.image').css('background-image', 'url("' + $(this).attr("data-picture") + '")');
    $('#logocard > div > div.content > div.meta > span')[0].innerHTML = $(this).attr("data-position");
    
    Shiny.setInputValue('main-player', $(this).attr("id"));
    Shiny.setInputValue('menu-level', 'player');
    $('#' + consts.dom.menu_navigation_id).find('.section')[0].classList.remove('active');
    changeView('player');
    window.scrollTo(0, 0);
  });
});
