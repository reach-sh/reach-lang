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
