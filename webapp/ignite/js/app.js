/*****
* CONFIGURATION
*/
    //Main navigation
    $.navigation = $('nav > ul.nav');

  $.panelIconOpened = 'icon-arrow-up';
  $.panelIconClosed = 'icon-arrow-down';

  //Default colours
  $.brandPrimary =  '#20a8d8';
  $.brandSuccess =  '#4dbd74';
  $.brandInfo =     '#63c2de';
  $.brandWarning =  '#f8cb00';
  $.brandDanger =   '#f86c6b';

  $.grayDark =      '#2a2c36';
  $.gray =          '#55595c';
  $.grayLight =     '#818a91';
  $.grayLighter =   '#d1d4d7';
  $.grayLightest =  '#f8f9fa';

'use strict';

/****
* MAIN NAVIGATION
*/

$(document).ready(function($){

  // Dropdown Menu 
  // Add dropdowns to sidebar
  $('.sidebar .nav-dropdown').has('ul').children('a').addClass('nav-dropdown-toggle');
	
  // Add class .active to current link
  

  // Add dropdown-toggle
/*  $('a.nav-dropdown-toggle').on('click', function (event) {
    e.stopPropagation();
    location.href = $(this).attr('href');
  });*/

  $.navigation.on('click', 'a', function(e){

    if ($.ajaxLoad) {
      e.preventDefault();
    }

    if ($(this).hasClass('nav-dropdown-toggle')) {
      resizeBroadcast();
    }

  });

  // Disable moving to top
  $('a[href="#"][data-top!=true]').click(function(e){
    e.preventDefault();
  });


  initNavToggle();  
  initTurboLinks();
  
  // Init JS
  toggleNavigation();
  initJSDefaults();
});

function resizeBroadcast() {

    var timesRun = 0;
    var interval = setInterval(function(){
      timesRun += 1;
      if(timesRun === 5){
        clearInterval(interval);
      }
      window.dispatchEvent(new Event('resize'));
    }, 62.5);

    return false;
  }

/* ---------- Open/Close navigation ------ */
function toggleNavigation(){
	/*
	$.navigation.find('.open, .active').each(function(){
		$(this).removeClass('open');
		$(this).removeClass('active');
	});*/
	
	$.navigation.find('a').each(function(){

	    var cUrl = String(window.location).split('?')[0];

	    if (cUrl.substr(cUrl.length - 1) == '#') {
	      cUrl = cUrl.slice(0,-1);
	    }

	    if ($($(this))[0].href==cUrl) {
	      $(this).addClass('active');

	      $(this).parents('ul').add(this).each(function(){
	        $(this).parent().addClass('open');
	      });
	    }
	  });
}

/* ---------- Init JS Frameworks ---------- */
function initJSDefaults(){
	  collapseFieldset();
      $('table.dataTable').dataTable();
	return false;
}

function destroyJSDefaults(){
    //Unbind
    $(".carousel").unbind();
    $('table.dataTable').dataTable().fnDestroy(); 
    return false;
}

function initTurboLinks(){
	  // Turbolinks
	  if(scipioStyles.turboLinks){
		  if(!$('body').hasClass('pace-extended'))$('body').addClass('pace-extended');
		  $('.side-nav a, .sidebar-nav a').click(function(e){
		    e.preventDefault();
		    var url = $(this).attr('href');
		    softLoadPage(url,'#content-main-section');
		  });
		}
}

/* ---------- Main Menu Open/Close, Min/Full ---------- */
function initNavToggle(){
	  $('.navbar-toggler').click(function(){

	    if ($(this).hasClass('sidebar-toggler')) {
	      $('body').toggleClass('sidebar-hidden');
	      resizeBroadcast();
	    }

	    if ($(this).hasClass('aside-menu-toggler')) {
	      $('body').toggleClass('aside-menu-hidden');
	      resizeBroadcast();
	    }

	    if ($(this).hasClass('mobile-sidebar-toggler')) {
	      $('body').toggleClass('sidebar-mobile-show');
	      resizeBroadcast();
	    }

	  });

	  $('.sidebar-close').click(function(){
	    $('body').toggleClass('sidebar-opened').parent().toggleClass('sidebar-opened');
	  });
}

/* ---------- Collapsible fields ---------- */
function collapseFieldset(){
    var parent = $(".toggleField");
    parent.each(function( index ) {
        $(this).find("fieldset > .row").wrapAll('<div class="collapse"/>'); 
    });
    

    $(".toggleField legend, .toggleField .legend").click(function(){
        $(this).children("i").toggleClass(" fa-arrow-right").toggleClass(" fa-arrow-down");
        $(this).nextAll("div.collapse").slideToggle(300);
    });
}

/****
* CARDS ACTIONS
*/

$(document).on('click', '.card-actions a', function(e){
  e.preventDefault();

  if ($(this).hasClass('btn-close')) {
    $(this).parent().parent().parent().fadeOut();
  } else if ($(this).hasClass('btn-minimize')) {
    var $target = $(this).parent().parent().next('.card-block');
    if (!$(this).hasClass('collapsed')) {
      $('i',$(this)).removeClass($.panelIconOpened).addClass($.panelIconClosed);
    } else {
      $('i',$(this)).removeClass($.panelIconClosed).addClass($.panelIconOpened);
    }

  } else if ($(this).hasClass('btn-setting')) {
    $('#myModal').modal('show');
  }

});

function capitalizeFirstLetter(string) {
  return string.charAt(0).toUpperCase() + string.slice(1);
}

function init(url) {

  /* ---------- Tooltip ---------- */
  $('[rel="tooltip"],[data-rel="tooltip"]').tooltip({"placement":"bottom",delay: { show: 400, hide: 200 }});

  /* ---------- Popover ---------- */
  $('[rel="popover"],[data-rel="popover"],[data-toggle="popover"]').popover();

}

/* no longer needed; macros can decide where since patch
jQuery(document).ready(function() {
    var asteriks = jQuery('.form-field-input-asterisk');
    asteriks.each(function() {
        var label = jQuery(this).parents('.form-field-entry').first().find('.form-field-label').first();
        label.append(' <span class="form-field-label-asterisk">*</span>');
    });
});
*/

/**
 * Object-fit fallback for IE (used by <@img> macro)
 * Based on:
 * https://medium.com/@primozcigler/neat-trick-for-css-object-fit-fallback-on-edge-and-other-browsers-afbc53bbb2c3#.cwwk7mtx0
 *
 * */
function scipioObjectFit(){
    Modernizr.addTest('objectfit',
            !!Modernizr.prefixed('objectFit')
        );
    if ( !Modernizr.objectfit ) {
        $('.scipio-image-container').each(function () {
            var $container = $(this),
                imgUrl = $container.find('img').prop('src');
            if (imgUrl) {
                var type= $container.attr('scipioFit');
                $container
                    .css('backgroundImage', 'url(' + imgUrl + ')')
                    .css('background-size',type)
                    .css('background-repeat','no-repeat')
                    .css('background-position','center center')
                    .addClass('compat-object-fit');
                $container.find('img').css('opacity','0');
            }
        });
        }
    return false;
}


function checkboxToFormHiddenInput(checkboxElem, formSel) {
    jQuery(formSel + ' input[name="' + checkboxElem.name + '"]').remove();
    if (checkboxElem.checked) {
        jQuery(formSel).append('<input type="hidden" name="' + checkboxElem.name + '" value="' + checkboxElem.value + '" />')
    }
}


function softLoadPage(url,target){
	Pace.restart();
	Pace.options.target = target;
	
	Pace.track(function () {
	    var params = {'scpRenderTargetExpr':'$Global-Column-Main','scpViewAsJson':'true'};
	    $(target).addClass('pace-wrap');
	    jQuery.ajax({
	        url: url,
	        type: 'POST',
	        data: params,
			failure: function (response) {
				$(target).removeClass('pace-wrap');
			},
			error: function (response) {
				$(target).removeClass('pace-wrap');
			},
	        success: function(data) {
	        	if(data.isLoggedIn){
		            var outElem = jQuery(target);
		            var renderOut;
		            
		            window.history.pushState({"html": '', "title":''},"", url);
		            
		            if (data._ERROR_MESSAGE_ || data._ERROR_MESSAGE_LIST_) {
		                // TODO: (data._ERROR_MESSAGE_LIST_)
		                if (data._ERROR_MESSAGE_) {
		                    renderOut = "ERROR MESSAGE: " + data._ERROR_MESSAGE_;
		                } else {
		                    renderOut = "ERROR MESSAGE (first from list): " + data._ERROR_MESSAGE_LIST_[0];
		                }
		            } else if (data.renderOut) {
		                renderOut = data.renderOut;
		            } else { 
		                renderOut = "NOTHING MATCHED OR UNRECOGNIZED ERROR";
		            }
		            destroyJSDefaults();
		            // Render
		            toggleNavigation();
		            $.navigation.find('.open, .active').each(function(){
		        		$(this).removeClass('open');
		        		$(this).removeClass('active');
		        	});
		            outElem.html(renderOut);
		            initJSDefaults();
		            scipio_boxify();
		            $(target).removeClass('pace-wrap');
	        	}else{
	        		location.reload(true);
	        	}
	        }
	    });
	});
	return false;
}