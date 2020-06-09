window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());

gtag('config', 'UA-168940826-1');

function try_ga(category, action, label, value) {
  try { // ad blockers can stop tracking the event
    gtag('event', action, {'event_category' : category, 'event_label' : label, 'value' : value});
  }
  catch (e) {
    console.error(e);
  }
}
