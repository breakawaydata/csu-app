window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());

gtag('config', 'UA-168940826-1');

function try_ga(action, category, label) {
  try { // ad blockers can stop tracking the event
    gtag('event', action, {'event_category' : category, 'event_label' : label});
  }
  catch (e) {
    console.error(e);
  }
}

function try_ga_message(message) {
  try_ga(message.action, message.category, message.label);
}

Shiny.addCustomMessageHandler('trackEvent', try_ga_message);
