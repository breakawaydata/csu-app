function changeView(container_id) {
  $('.' + consts.dom.body_class)
      .find('.' + consts.dom.body_container_class)
      .not('#' + container_id + '-container')
      .hide();
  $('#' + container_id + '-container')
    .show();
}

$( document ).ready(function() {
  let navigation_selector = "#" + consts.dom.menu_navigation_id;
  $(document).on('click', navigation_selector + ' > .section', function(){

    $(this)
      .addClass('active')
      .closest(navigation_selector)
      .find('.section')
        .not($(this))
        .removeClass('active')
    ;
    
    let button_id = $(this).attr('data-value');
    changeView(button_id);
    
    $("#logocard > div > div.content > div.header")[0].innerHTML = consts.global.team_name;
    $('#logocard > div > div.image').css('background-image', 'url("' + consts.global.team_logo + '")');
    if(button_id == 'all') {
      $("#logocard > div > div.content > div.meta > span")[0].innerHTML = "All players";
    } else {
      $("#logocard > div > div.content > div.meta > span")[0].innerHTML = "Positions";
    }
    
    Shiny.setInputValue('menu-level', button_id);

  });
});
