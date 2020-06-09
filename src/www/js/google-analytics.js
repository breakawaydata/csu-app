window.dataLayer = window.dataLayer || [];
function gtag(){dataLayer.push(arguments);}
gtag('js', new Date());

gtag('config', 'UA-168940826-1');

function try_ga(category, action, label, value) {
  try { // ad blockers can stop tracking the event
    ga('send', 'event', category, action, label, value);
  }
  catch (e) {
    console.error(e);
  }
}