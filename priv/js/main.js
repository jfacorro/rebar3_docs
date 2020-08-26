var navigation = true;

var toggleNav = function() {
  changeClass(navigation? "no-nav" : "nav");
  navigation = !navigation;
};

var changeClass = function(className) {
  let ids = ["sidenav", "main", "toggle-nav-button"];
  for(id of ids) {
    document.getElementById(id).className = className;
  }
};

var isMatch = function(x, y) {
  return x.toLowerCase().indexOf(y.toLowerCase()) != -1;
};

var autocomplete = function(input) {
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

  var highlightQuery = function(text, query) {
    return text.replace(new RegExp(query, "i"), function(match) {
      return "<strong>" + match + "</strong>";
    });
  };

  var createResultList = function(parent, result, query) {
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
      itemDiv.innerHTML = highlightQuery(item.name, query);
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
    createResultList(this.parentNode, find(query), query);

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
};

var menu = function() {
  var findCurrentItem = function(items, filename) {
    for(item of items) {
      var a = item.getElementsByTagName("a");
      if(a[0].attributes.href.value === filename) {
        return item;
      }
    }
    return null;
  };

  var addChildren = function(item, children) {
    var ul = document.createElement("ul");
    for(child of children) {
      var li = document.createElement("li");
      li.innerHTML = "<div class='label'><a href='" + child.href + "'>" + child.title + "</a></div>";
      if(typeof(child.children) != "undefined") {
        addChildren(li, child.children);
      }
      ul.appendChild(li);
    }
    item.appendChild(ul);
  };

  var expandModuleItem = function(item, filename, ref) {
    var module = filename.substring(0, filename.lastIndexOf("."));
    var node = navTree[module];
    var children =
        [ { "href": filename + "#top", "title": "Top"},
          { "href": filename + "#summary", "title": "Summary"}
        ];
    if(node.types.length > 0) {
      var types = {
        "href": filename + "#types",
        "title": "Types",
        "children": []
      };
      for(t of node.types) {
        types.children.push({"href": filename + "#" + t, "title": t});
      }
      children.push(types);
    }
    if(node.functions.length > 0) {
      var functions = {
        "href": filename + "#functions",
        "title": "Functions",
        "children": []
      };
      for(f of node.functions) {
        functions.children.push({"href": filename + "#" + f, "title": f});
      }
      children.push(functions);
    }
    addChildren(item, children);
  };

  var links = document.getElementById('sidenav-links');
  var modules = document.getElementById('sidenav-modules');

  var url = window.location.pathname;
  var filename = url.substring(url.lastIndexOf('/') + 1);
  var ref = window.location.hash.substring(1);

  var all = Array.from(links.getElementsByTagName("li"))
      .concat(Array.from(modules.getElementsByTagName("li")));

  var currentItem = findCurrentItem(all, filename);
  currentItem.classList.add("selected");
  var isModule = currentItem.parentNode === modules;
  if(isModule) {
    modules.scrollTop = currentItem.offsetTop - modules.offsetTop - 40;
    expandModuleItem(currentItem, filename, ref);
  }
};

var ready = function(f) {
  document.addEventListener('DOMContentLoaded', f);
};

ready(function(e) {
  var search = document.getElementById('search-query');
  autocomplete(search);
  menu();
});
