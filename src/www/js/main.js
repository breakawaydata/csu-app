$( document ).ready(function() {
  $('.position-card').click(function(){

    let position = $(this).attr('data-position');
    $('#players').search('set value', position).search('search remote', position);
    $('.ui.breadcrumb').find('.section')[0].click();

  });
});
