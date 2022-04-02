exports._resize = resize;

function resize(window, document, source, target, type) {

  var handle = document.querySelector(source);
  var box    = document.querySelector(target);

  handle.addEventListener('mousedown', initialiseResize, false);

  function initialiseResize(e) {
    window.addEventListener('mousemove', startResizing, false);
    window.addEventListener('mouseup', stopResizing, false);
  }

  function startResizing(e) {
    if (type === 'both' || type === 'horizontal')
      box.style.height = (e.clientY - box.offsetTop) + 'px';
    if (type === 'both' || type === 'vertical')
      box.style.width = (e.clientX - box.offsetLeft) + 'px';

    // prevent "user-select" highlights
    document.body.classList.add('no-user-select');
  }

  function stopResizing(e) {
    window.removeEventListener('mousemove', startResizing, false);
    window.removeEventListener('mouseup', stopResizing, false);

    box.classList.remove('no-user-select');
  }
}
