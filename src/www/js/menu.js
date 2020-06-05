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
    
    Shiny.setInputValue('menu-level', button_id);

  });
});
