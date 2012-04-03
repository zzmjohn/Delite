// Override the onload event
$(document).ready(function(){
    toggleGenFiles();
//    displaySymbols();
});


function toggleGenFiles(){
  $(".gen-sourcefile-header").each(function (index, domEle){
    //  alert("whaaa");
    $(domEle).click(function(){
      $(domEle).next().toggle();
    });    
  });
}
