$(document.body).ready(function() {
    //find first span after pre and use it as top-level node
    var lastSelectedNode = null;
    var keepResults = false;
    var timeoutId = null;

    $("#parse_tree_output").html("Found " + $("span").size() + " node(s)");

    function updateTree(node) {
        if(lastSelectedNode) {
            $(lastSelectedNode).css("border","");
            $(lastSelectedNode).css("background-color","");
        }
        $(node).css("border","1px solid orange");
        $(node).css("background-color","#FFF5DF");
        lastSelectedNode = node;
        var output = "";
        var ident = "";
        var rules = $("#" + node.id.replace("node","tree")).text().split(/ /);
        for(var i = 0; i < rules.length; i++) {
            var r = rules[i];
            output += ident + '<span class="' + r + '">' + r + '</span><br/>';
                ident += "&nbsp;";
        }
        $("#parse_tree_output").html(output);
    }

    function bind_node_highlighter() {
        $(document.body).mousemove(function(e) {
            if(keepResults) {
                return false;
            }
            var node =  $.browser.msie ? e.srcElement : e.target;
            if(node.nodeName == "PRE") {
                if(lastSelectedNode) {
                    $(lastSelectedNode).css("border","");
                    $(lastSelectedNode).css("background-color","");
                    lastSelectedNode = null;
                }
                $("#parse_tree_output").html("... ");
                return false;
            }
            if (lastSelectedNode == node || node.nodeName != "SPAN") {
                return false;
            }
            
            clearTimeout(timeoutId);
            timeoutId = setTimeout(function() { updateTree(node) }, 100);
            return false;
        });  
    }
    
    function bind_clicks() {
        $(document.body).click(function(e) {
            var node =  $.browser.msie ? e.srcElement : e.target;
            if(node.nodeName == "SPAN") {
                if(keepResults) {
                    if(lastSelectedNode) {
                        $(lastSelectedNode).css("border","");
                        $(lastSelectedNode).css("background-color","");
                    }
                } else {
                    $(node).css("border","1px solid black");
                    lastSelectedNode = node;
                }
                keepResults = !keepResults;
            } else {
                if(lastSelectedNode) {
                        $(lastSelectedNode).css("border","");
                        $(lastSelectedNode).css("background-color","");
                }
                keepResults = false;
            }
            return false;
        });
     }
    
    function bind_events() {
        bind_node_highlighter();
        bind_clicks();
    }

    $(window).scroll(function() {
        $("#parse_tree").css("top", "" + document.body.scrollTop + "px");
    });
    
    function unbind_events() {
        $("#parse_tree_output").html("Unbinding events... Please wait");
        $(document.body).unbind();
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
            "\nNote: Some browsers exhibit problems with unloading pages with too many nodes. "+
            "If you encounter that simply wait for it or kill it."
        );
    });

});

