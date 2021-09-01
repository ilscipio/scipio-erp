/**
 * SCIPIO: jquery plugin to add a class to subsets of html elements.
 * Used to add classes to elements, so that they can be styled as boxes.
 * */
var elSelector = 'pre.scrollable,.section-screenlet-content';
var notSubSelector = 'pre.scrollable,.section-screenlet-content,.btn-group,.steps,.tile-container, .signup-panel,.button-group,.scipio-image-container';
var noInnerSubSelector = 'scrollable';
var scipioBoxClass = 'card';
var scipioBoxSubClass = 'card-block';
var scipioBoxHeaderClass = 'card-header';


function scipio_boxify(){
    $(elSelector).each(function(){
        if($(this).has(notSubSelector).length == 0 && $(this).text().trim().length > 0){
        	var cell = $(this);
            cell.addClass(scipioBoxClass);
            if(cell.not(noInnerSubSelector)){
            	var innerCell = $('<div/>')
            	.addClass(scipioBoxSubClass);
            	cell.wrapInner(innerCell);
            }
            var headElement = innerCell.find('h1,h2,h3,h4,h5,h6').addClass('card-title');
        }
    });
}

$(function(){
    scipio_boxify();
});

