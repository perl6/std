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
    $("span").mouseover(function() {
        var ruleName = this.className;
        var propogateEvent = true;
        var i,r;
        var parseTreeOutput;
        if(rules.length == 0) {
            //last leaf node...
            if(lastSelectedNode) {
                $(lastSelectedNode).css("border","");
            }
            $(this).css("border","1px solid black");
            lastSelectedNode = this;                
        }
        rules.push(ruleName);
        if(ruleName == topLevelRuleName) {
            parseTreeOutput = "";
            ident = "";
            for(i = rules.length - 1; i >= 0; i--) {
                r = rules[i];
                parseTreeOutput += ident + '<span class="' + r + '">' + r + '</span><br/>';
                ident += "&nbsp;";
            }
            $("#parseTreeOutput").html(parseTreeOutput);
            rules = [];
            propogateEvent = false;
        } 
        return propogateEvent;
    });

});

