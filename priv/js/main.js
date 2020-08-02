function openNav() {
  changeClass("nav");
}

function closeNav() {
  changeClass("no-nav");
}

function changeClass(className) {
  let ids = ["sidenav", "main", "show-nav-button"];
  for(id of ids) {
    document.getElementById(id).className = className;
  }
}
