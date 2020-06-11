$( document ).ready(function() {
  $('.' + consts.dom.position_card_class).click(function(){

    let position = $(this).attr('data-position');
    $('#' + consts.search.id).search('set value', position).search('search remote', position);
    
    // switch to All view
    $("#" + consts.dom.menu_navigation_id + " > .section").removeClass('active');
    $("#" + consts.dom.menu_navigation_id + " > .section:first").addClass('active');
    changeView(consts.dom.all_level_id);
    changeLogoCard(consts.dom.all_level_id);
    Shiny.setInputValue('menu-level', consts.dom.all_level_id);
  });
});
