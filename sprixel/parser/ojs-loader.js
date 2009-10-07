/*
  This software is copyright 2009 Matthew Wilson matthew@wilson.org
  and available under the Artistic License 2 ("AL2") as documented at
  http://jsmeta.googlecode.com
*/


var printer;
function Print(outputText) {
  if (typeof(outputText)=='undefined') {
    return;
  }
  printer.innerHTML += (outputText.toString() + "\n").replace(/\n/g,'<br />').replace(/\t/g,'&nbsp;&nbsp;&nbsp;&nbsp;');
};

if (inConsole) {
  Print = print;
  (function() {
    if (typeof(TestMode)!='undefined') {
      Print(runTests());
    } else {
      Print(runMGrammar());
    }
  })();
} else {
  var addEvent = function(eName, runner) {
    if (window.addEventListener) {
      window.addEventListener(eName, runner, false);
    } else {
      window.attachEvent('on'+eName, runner);
    }
  }
  addEvent('load', function initializeParser() {
    printer = Dom('output');
    printer.innerHTML = '<hr />';
    if (typeof(TestMode)!='undefined') {
      Dom('result').innerHTML = runTests().replace(/\n/g,'<br />').replace(/\t/g,'&nbsp;&nbsp;&nbsp;&nbsp;');
    } else {
      Dom('result').innerHTML = runMGrammar();
    }
  });
}
































