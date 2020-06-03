function replaceContent(selector, data_attribute) {
  $(selector).each(function( index ) {
    let new_value = Math.round(+$(this).attr('data-' + data_attribute));
    let stat_element = $(this).find('.stat');
    stat_element.text(new_value);
  })
}

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
    
    let stat_id = $(this).attr('id');
    replaceContent('.player-card', stat_id);
    replaceContent('.position-card', stat_id);

  });
});
