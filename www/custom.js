document.addEventListener('DOMContentLoaded', function() {
  document.querySelectorAll('.sidebar .nav-item .nav-link').forEach(function(el) {
    if (el.textContent.includes('?')) {
      el.addEventListener('click', function() {
        alert('Activaste el botón de ayuda!');
      });
    }
  });
});
