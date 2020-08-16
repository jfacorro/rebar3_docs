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

function autocomplete(input) {
  const MAX_RESULT_COUNT = 5;
  const DOM_KEY_DOWN = 40;
  const DOM_KEY_UP = 38;
  const DOM_KEY_ENTER = 13;
  const DOM_KEY_ESCAPE = 27;

  const navItems = navTree;
  var currentItemIndex = -1;

  var findMatches = function(result, query, items, type, module) {
    for(item of items) {
      if(result.length >= MAX_RESULT_COUNT) {
        return;
      }
      if(isMatch(item, query)) {
        result.push({
          "name": item,
          "type": type,
          "module": type != "module" ? module : item
        });
      }
    }
  };

  var find = function(query) {
    var result = [];
    findMatches(result, query, Object.keys(navItems), "module");
    for(moduleName in navItems) {
      var module = navItems[moduleName];
      findMatches(result, query, module.functions, "function", moduleName);
      findMatches(result, query, module.types, "type", moduleName);
    }
    return result;
  };

  var createResultList = function(parent, result) {
    var div = document.createElement("div");
    div.setAttribute("id", "autocomplete-items");
    div.setAttribute("class", "autocomplete-items");
    parent.appendChild(div);
    for(item of result) {
      var itemLink = document.createElement("a");
      var href = item.module + ".html" + (item.type == "module" ? "" : "#" + item.name);
      itemLink.setAttribute("href", href);

      var itemDiv = document.createElement("div");
      itemDiv.setAttribute("class", "autocomplete-item");
      itemDiv.innerHTML = item.name;
      itemDiv.innerHTML += item.type == "type" ? " (type)" : "";
      itemDiv.innerHTML += item.type != "module" ? "<span>" + item.module + "</span>" : "";
      itemLink.appendChild(itemDiv);
      div.appendChild(itemLink);
    }
  };

  var deleteResultList = function() {
    currentItemIndex = -1;
    for(x of document.getElementsByClassName("autocomplete-items")) {
      x.parentNode.removeChild(x);
    }
  };

  var close = function(search) {
    search.value = "";
    deleteResultList();
  };

  var run = function(e) {
    var query = this.value;
    if (!query) { return false; }

    deleteResultList();
    createResultList(this.parentNode, find(query));

    return true;
  };

  var navigate = function(e) {
    var items = document.getElementById("autocomplete-items");
    if (items) {
      items = items.getElementsByClassName("autocomplete-item");
      switch(e.keyCode) {
        case DOM_KEY_DOWN:   currentItemIndex++; break;
        case DOM_KEY_UP:     currentItemIndex--; break;
        case DOM_KEY_ENTER:  e.preventDefault(); break;
        case DOM_KEY_ESCAPE: close(this); return;
        default: return;
      }
      if(e.keyCode == DOM_KEY_ENTER) {
        if(currentItemIndex > -1) {
          items[currentItemIndex].click();
          deleteResultList();
        }
      } else {
        updateActiveItem(items);
      }
    }
  };

  var updateActiveItem = function(items) {
    for (item of items) { item.classList.remove("autocomplete-active"); }
    if (currentItemIndex >= items.length) currentItemIndex = 0;
    if (currentItemIndex < 0) currentItemIndex = (items.length - 1);
    items[currentItemIndex].classList.add("autocomplete-active");
  };

  input.addEventListener('input', run);
  input.addEventListener('keydown', navigate);
}

function ready(f) {
  document.addEventListener('DOMContentLoaded', f);
}

ready(function(e) {
  var search = document.getElementById('search-query');
  autocomplete(search);
});
