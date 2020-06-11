$( document ).ready(function() {
  $('.' + consts.dom.position_card_class).click(function(){

    let position = $(this).attr('data-position');
    $('#' + consts.search.id).search('set value', position).search('search remote', position);
    
    switchToAllView();
  });
});
