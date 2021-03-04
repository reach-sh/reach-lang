function reach_DoSearch(event, field) {
  var val = field.value;
  if ( event && event.key === 'Enter' ) {
    window.location = "https://www.google.com/search?q=site:docs.reach.sh+" + encodeURIComponent(val);
    return false;
  }
  return true;
}

function reach_init() {
  var clipboard = new ClipboardJS('.btn');

  clipboard.on('success', function(e) {
    e.clearSelection();
    showTooltip(e.trigger, 'Copied!');
  });
  clipboard.on('error', function(e) {
    showTooltip(e.trigger, fallbackMessage(e.action));
  });
}
