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
    div.setAttribute("id", "autocomplete-items");
    div.setAttribute("class", "autocomplete-items");
    parent.appendChild(div);
    for(item of result) {
      var itemLink = document.createElement("a");
      var href = item.module + ".html" + (item.type == "module" ? "" : "#" + item.name);
      itemLink.setAttribute("href", href);

      var itemDiv = document.createElement("div");
      itemDiv.setAttribute("class", "autocomplete-item");
      itemDiv.innerHTML = item.name + (item.type == "type" ? " (type)" : "");
      itemDiv.innerHTML += (item.type == "module" ? "" : "<br/>" + item.module);
      itemLink.appendChild(itemDiv);
      div.appendChild(itemLink);
    }
  };

  var deleteResultList = function() {
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

  var DOM_KEY_DOWN = 40;
  var DOM_KEY_UP = 38;
  var DOM_KEY_ENTER = 13;
  var DOM_KEY_ESCAPE = 27;
  var currentFocus = -1;

  var navigate = function(e) {
    var items = document.getElementById("autocomplete-items");
    if (items) {
      items = items.getElementsByClassName("autocomplete-item");
      switch(e.keyCode) {
        case DOM_KEY_DOWN:   currentFocus++; break;
        case DOM_KEY_UP:     currentFocus--; break;
        case DOM_KEY_ENTER:  e.preventDefault(); break;
        case DOM_KEY_ESCAPE: close(this); return;
        default: return;
      }
      if(e.keyCode == DOM_KEY_ENTER) {
        if(currentFocus > -1) {
          items[currentFocus].click();
        }
      } else {
        updateActiveItem(items);
      }
    }
  };

  var updateActiveItem = function(items) {
    for (item of items) { item.classList.remove("autocomplete-active"); }
    if (currentFocus >= items.length) currentFocus = 0;
    if (currentFocus < 0) currentFocus = (items.length - 1);
    items[currentFocus].classList.add("autocomplete-active");
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
