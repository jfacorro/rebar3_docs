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

function isMatch(x, y) {
  return x.toLowerCase().indexOf(y.toLowerCase()) != -1;
}

function findMatches(result, query, items, type, module) {
  for(item of items) {
    if(isMatch(item, query)) {
      result.push({
        "name": item,
        "type": type,
        "module": type != "module" ? module : item
      });
    }
  }
}

function autocomplete(input) {
  var items = navTree;

  var find = function(query) {
    var result = [];
    findMatches(result, query, Object.keys(items), "module");
    for(moduleName in items) {
      var module = items[moduleName];
      findMatches(result, query, module.functions, "function", moduleName);
      findMatches(result, query, module.types, "type", moduleName);
    }
    return result;
  };

  var createResultList = function(parent, result) {
    var div = document.createElement("div");
    div.setAttribute("class", "autocomplete-items");
    parent.appendChild(div);
    for(item of result) {
      var itemDiv = document.createElement("div");
      var itemLink = document.createElement("a");
      var href = item.module + ".html" + (item.type == "module" ? "" : "#" + item.name);
      itemLink.setAttribute("href", href);
      itemLink.innerHTML = item.name + (item.type == "type" ? " (type)" : "");
      itemLink.innerHTML += (item.type == "module" ? "" : "<br/>" + item.module);
      itemDiv.appendChild(itemLink);
      div.appendChild(itemDiv);
    }
  };

  var deleteResultList = function() {
    for(x of document.getElementsByClassName("autocomplete-items")) {
      x.parentNode.removeChild(x);
    }
  };

  var run = function(e) {
    var query = this.value;
    if (!query) { return false; }

    deleteResultList();
    createResultList(this.parentNode, find(query));

    return true;
  };

  input.addEventListener('input', run);
}

function ready(f) {
  document.addEventListener('DOMContentLoaded', f);
}

ready(function(e) {
  var search = document.getElementById('search-query');
  autocomplete(search);
});
