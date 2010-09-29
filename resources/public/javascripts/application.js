$(function() {
  SyntaxHighlighter.highlight(); 

  $('.toggle a').click(function(e) {
    $(this).parents('.toggle').children().toggle();
  });

  link('code.clojure.functions');
  link('code.clojure.preprocessor');
});

/**
 * Add search links to elements defied by selector 'sel'.
 */
var link = function(sel) {
  $.each($(sel), function(i, e) {
    var je = $(e);
    je.wrap('<a href="/search?mode=is&query=' + je.text() + '"></a>').attr('style', 'text-decoration:underline');
  });

};

