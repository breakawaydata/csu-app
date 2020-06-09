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
    
    $('#' + consts.dom.logo_card_id + ' div.header')[0]
      .innerHTML = consts.global.team_name;
    $('#' + consts.dom.logo_card_id + ' div.image')
      .css('background-image', 'url("' + consts.global.team_logo + '")');
    let meta_text = (button_id == consts.dom.all_level_id) ? "All players" : "Positions";
    $('#' + consts.dom.logo_card_id + ' div.meta > span')[0]
      .innerHTML = meta_text;
    
    changeView(button_id);
    
    Shiny.setInputValue('menu-level', button_id);

  });
});

function updateUserCard(message) {
  let avatar_link = consts.global.gravatar_url + md5(message.username) + '?s=80&d=mm'
  $('.' + consts.dom.user_card_class + ' img').attr('src', avatar_link);
  $('.' + consts.dom.user_card_class + ' .sub.header').html(message.username);
}

Shiny.addCustomMessageHandler("update_user_data", updateUserCard);
