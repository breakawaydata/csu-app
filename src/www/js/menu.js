// _todo_ example of js function, to be removed/modified
/*
$( document ).ready(function() {
  $(document).on('click','#menu > .item', function(){
    
    Shiny.setInputValue('active_level', $(this).attr("id"));

    $(this)
      .addClass('active')
      .closest('.ui.menu')
      .find('.item')
        .not($(this))
        .removeClass('active')
    ;
    
    let button_id = $(this).attr('id');
    Shiny.setInputValue('menu-active', button_id);

  });
});
*/
$( document ).ready(function() {
  $(document).on('click', '.ui.breadcrumb > .section', function(){

    $(this)
      .addClass('active')
      .closest('.ui.breadcrumb')
      .find('.section')
        .not($(this))
        .removeClass('active')
    ;
    
    let button_id = $(this).attr('data-value');
    $('.main')
      .find('.body-container')
      .not('#' + button_id + '-container')
      .hide()
    $('#' + button_id + '-container')
      .show()
    
    Shiny.setInputValue('menu-level', button_id);

  });
});
