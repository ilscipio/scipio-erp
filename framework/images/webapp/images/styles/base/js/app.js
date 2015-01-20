// Foundation JavaScript
// Documentation can be found at: http://foundation.zurb.com/docs

function collapseFieldset(){
	$( ".toggleField .row" ).wrapAll( "<div class='collapsehide' style='display:none'/>");
	$(".toggleField legend").click(function(){
        $(this).children("i").toggleClass(" fa-arrow-right").toggleClass(" fa-arrow-down");
        $(this).nextAll("div.collapsehide").slideToggle(500);
	});
}


$(function(){
	$(document).foundation();
	collapseFieldset();
});




