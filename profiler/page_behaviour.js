// Override the onload event
$(document).ready(function(){
 $(".gen-sourcefile-header").each(function (index, domEle){
   //  alert("whaaa");
    $(domEle).click(function(){
      $(domEle).next().toggle();
    });    
 });
 
 
 // the page finished loading, do something ...
});
