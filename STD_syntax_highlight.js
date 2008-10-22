$(document.body).ready(function() {
    //find first span after pre and use it as top-level node
    var topLevelRuleName = "";
    var lastSelectedNode;
    var rules;
    var keepResults = false;

    $("pre > span").each(function(index,value) {
        topLevelRuleName = this.className;
    });
    
    if(topLevelRuleName == "") {
        alert("Assertion: Top Level node (pre > span) could not be found...");
        return;
    }
    $("#parse_tree_output").html("Found " + $("span").size() + " node(s)");

    function bind_body_mouseover() {
        lastSelectedNode = null;
        $("body").mouseover(function(e) {
            if(keepResults) {
                return;
            }
            
            if(lastSelectedNode) {
                $(lastSelectedNode).css("border","");
                $(lastSelectedNode).css("background-color","");
            }
            $("#parse_tree_output").html("...");
        });
    }
    
    function bind_span_mouseover() {
        rules = [];
        $("span").mouseover(function(e) {
            if(keepResults) {
                return;
            }
            
            var ruleName = this.className;
            var propogateEvent = true;
            var i,r;
            var output;
            if(rules.length == 0) {
                //last leaf node...
                if(lastSelectedNode) {
                    $(lastSelectedNode).css("border","");
                    $(lastSelectedNode).css("background-color","");
                }
                $(this).css("border","1px solid orange");
                $(this).css("background-color","#FFF5DF");
                lastSelectedNode = this;
            }
            rules.push(ruleName);
            if(ruleName == topLevelRuleName) {
                output = "";
                ident = "";
                for(i = rules.length - 1; i >= 0; i--) {
                    r = rules[i];
                    output += ident + '<span class="' + r + '">' + r + '</span><br/>';
                    ident += "&nbsp;";
                }
                $("#parse_tree_output").html(output);
                rules = [];
                propogateEvent = false;
            }
            return propogateEvent;
        });
    }
    
    function bind_clicks() {
        $("span").click(function(e) {
            if(keepResults) {
                if(lastSelectedNode) {
                    $(lastSelectedNode).css("border","");
                    $(lastSelectedNode).css("background-color","");
                }
            } else {
                $(this).css("border","1px solid black");
                lastSelectedNode = this;
            }
            keepResults = !keepResults;
            return false;
        });
        $("body").click(function(e) {
            if(keepResults) {
                if(lastSelectedNode) {
                    $(lastSelectedNode).css("border","");
                    $(lastSelectedNode).css("background-color","");
                }
                keepResults = false;
            }
        });
    
    }
    
    function bind_events() {
        bind_body_mouseover();
        bind_span_mouseover();
        bind_clicks();
    }

    $(window).scroll(function() {
        $("#parse_tree").css("top", "" + document.body.scrollTop + "px");
    });
    
    function unbind_events() {
        $("#parse_tree_output").html("Unbinding events... Please wait");
        $("span").unbind();
        $("body").unbind();
    }
    
    $("#parse_tree_collapse").hide();
    
    $("#parse_tree_expand").click(function() {
        bind_events();
        $("#parse_tree_output").show();
        $("#parse_tree_expand").hide();
        $("#parse_tree_collapse").show();
        $("#parse_tree_output").html("...");
    });
    $("#parse_tree_collapse").click(function() {
        unbind_events();
        $("#parse_tree_output").hide();
        $("#parse_tree_expand").show();
        $("#parse_tree_collapse").hide();
        $("#parse_tree_output").empty();
    });
    $("#parse_tree_help").click(function() {

        alert(
            "**** Help ****\n" +
            "\n1. Click on the 'Show Syntax Tree' button, and then hover over the Perl 6 code to see its syntax tree.\n" +
            "\n2. Click on the highlighted code if you need to keep the results.\n" +
            "Click again anywhere and this behavior will be reset.\n" +
            "\nNote: Some browsers exibit problems with unloading pages with too many nodes. "+
            "If you encounter that simply wait for it or kill it."
        );
    });

});

