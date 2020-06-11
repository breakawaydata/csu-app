function replaceContent(selector, data_attribute) {
  $(selector).each(function( index ) {
    let new_value = Math.round(+$(this).attr('data-' + data_attribute));
    let stat_element = $(this).find('.stat');
    stat_element.text(new_value);
  });
}

$( document ).ready(function() {
  $(document).on('click','#' + consts.dom.sidebar_navigation_id + ' > .item', function(){
    
    Shiny.setInputValue('sidebar-stat', $(this).attr("id"));
    
    $(this)
      .addClass('sidebar-active')
      .closest('#' + consts.dom.sidebar_navigation_id)
      .find('.item')
        .not($(this))
        .removeClass('sidebar-active')
    ;
    
    let stat_id = $(this).attr('id');
    try_ga('click', 'statistic', stat_id);
    
    replaceContent('.' + consts.dom.player_card_class, stat_id);
    var $all_container = $('#' + consts.dom.body_container_all_id);
    $all_container.find('.' + consts.dom.player_card_class).sort(function(a, b) {
      return +b.getAttribute('data-' + stat_id) - +a.getAttribute('data-' + stat_id);
    }).appendTo($all_container);
    
    replaceContent('.' + consts.dom.position_card_class, stat_id);
    var $position_container = $('#' + consts.dom.body_container_position_id);
    $position_container.find('.' + consts.dom.position_card_class).sort(function(a, b) {
      return +b.getAttribute('data-' + stat_id) - +a.getAttribute('data-' + stat_id);
    }).appendTo($position_container);

  });
});
