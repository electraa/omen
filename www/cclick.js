var circles = document.getElementsByTagName("circle");
    var nodes = document.getElementsByClassName("node");
              for (var i = 1; i < circles.length; i++) {
              circles[i].addEventListener("click", function(e) {
              for (var j = 1; j < nodes.length; j++) {
              if (nodes[j].children[1]==e.currentTarget){
                alert("fgs");
              Shiny.onInputChange("mdf", nodes[j]);
              }
              }
              })
              circles[i].addEventListener("mouseover", function(e) {
              e.currentTarget.style.opacity = "0.7";
              })
              circles[i].addEventListener("mouseout", function(e) {
              e.currentTarget.style.opacity = "1";})
              }