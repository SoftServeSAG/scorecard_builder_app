function showBigImg()
{
    var d = document.getElementById("img-pop-up");
    d.setAttribute("style", "display:block;");

}

function hideBigImg()
{
    var d = document.getElementById("img-pop-up");
    d.setAttribute("style", "display:none;");
}

function view(imgsrc) {
      viewwin = window.open(imgsrc,'viewwin', 'width=600,height=300'); 
}

Shiny.addCustomMessageHandler("hud",
  function(message) {
    if (message == "show") {
      showBigImg();
    } else {
      hideBigImg();
    }
  }
);
