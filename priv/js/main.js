function openNav() {
  changeClass("nav");
}

function closeNav() {
  changeClass("no-nav");
}

function changeClass(className) {
  document.getElementById("sidenav").className = className;
  document.getElementById("main").className = className;
}
