// Foundation JavaScript
// Documentation can be found at: http://foundation.zurb.com/docs

function collapseFieldset(){
	$( ".toggleField .row" ).wrapAll( "<div class='hide' />");
	$(".toggleField legend").click(function(){
        $(this).children("i").toggleClass(" fa-arrow-right").toggleClass(" fa-arrow-down");
        $(this).nextAll("div.hide").slideToggle(500);
	});
}


$(function(){
	$(document).foundation();
	collapseFieldset();
});




