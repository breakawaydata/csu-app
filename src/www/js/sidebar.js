$( document ).ready(function() {
  $(document).on('click','#sidebar > .item', function(){
    
    Shiny.setInputValue('sidebar-stat', $(this).attr("id"));
    
    $(this)
      .addClass('sidebar-active')
      .closest('.ui.menu')
      .find('.item')
        .not($(this))
        .removeClass('sidebar-active')
    ;

  });
});
