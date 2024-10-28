// assets/script.js
function updateMode() {
    const isDark = localStorage.getItem('theme') === 'dark';
    document.documentElement.classList.toggle("dark", isDark);
    document.body.style.backgroundColor = isDark ? "#333" : "#ffffff";
    document.body.style.color = isDark ? "#ffffff" : "#000000";
}

function toggleMode() {
    const newTheme = localStorage.getItem('theme') === 'dark' ? 'light' : 'dark';
    localStorage.setItem('theme', newTheme);
    updateMode();
}

window.onload = function() {
    if (!localStorage.getItem('theme')) {
        localStorage.setItem('theme', 'light');
    }
    updateMode();
};
