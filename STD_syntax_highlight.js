$(document.body).ready(function() {
    //find first span after pre and use it as top-level node
    topLevelRuleName = "";
    $("pre > span").each(function(index,value) {
        if(index == 0) {
            topLevelRuleName = this.className;
        }
    });
    if(topLevelRuleName == "") {
        alert("Assertion: Top Level node (pre > span) could not be found..."); 
        return;
    }
    
    var rules = [];
    var lastSelectedNode = null;
    $("span").mouseover(function(e) {
        $("span").css("border","");
        $("span").css("background-color","");
        $(this).css("border","1px solid orange");
        $(this).css("background-color","#FFF5DF");
        e.stopPropagation();
    });

});

