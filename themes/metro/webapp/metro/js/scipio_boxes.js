/**
 * SCIPIO: jquery plugin to add a class to subsets of html elements.
 * Used to add classes to elements, so that they can be styled as boxes.
 * */
var elSelector = 'pre.scrollable,.section-screenlet-content';
var notSubSelector = 'pre.scrollable,.section-screenlet-content, table,.orbit-container, .steps, .tile-container, .signup-panel,.button-group,.trumbowyg-box';
var scipioBoxClass = 'box';


function scipio_boxify(){
    $(elSelector).each(function(){
        if($(this).has(notSubSelector).length == 0 && $(this).text().trim().length > 0){
            $(this).addClass(scipioBoxClass);
        }
    });
}

$(function(){
    scipio_boxify();
});

