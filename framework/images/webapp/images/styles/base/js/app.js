// Foundation JavaScript
// Documentation can be found at: http://foundation.zurb.com/docs

function collapseFieldset(){
	var parent = $(".toggleField");
	parent.each(function( index ) {
		$(this).find("fieldset .row").wrapAll( "<div class='collapsehide'/>");
		if(parent.hasClass("collapsed")){
			parent.find(".collapsehide").hide();
		} 
	});
	

	$(".toggleField legend, .toggleField .legend").click(function(){
        $(this).children("i").toggleClass(" fa-arrow-right").toggleClass(" fa-arrow-down");
        $(this).nextAll("div.collapsehide").slideToggle(300);
	});
}


$(function(){
	$(document).foundation();
	collapseFieldset();
	$('.ui-autocomplete').addClass('f-dropdown');
	// Pizza.init(); Create charts
});




