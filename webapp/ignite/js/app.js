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

  // Add dropdowns to sidebar
  $('.sidebar .nav-dropdown').has('ul').children('a').addClass('nav-dropdown-toggle');
	
  // Add class .active to current link
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
  
  // Add slider elements

  // Dropdown Menu
  $.navigation.on('click', 'a', function(e){

    if ($.ajaxLoad) {
      e.preventDefault();
    }

    if ($(this).hasClass('nav-dropdown-toggle')) {
      //$(this).parent().toggleClass('open');
      resizeBroadcast();
    }

  });
  
  // Other

  function resizeBroadcast() {

    var timesRun = 0;
    var interval = setInterval(function(){
      timesRun += 1;
      if(timesRun === 5){
        clearInterval(interval);
      }
      window.dispatchEvent(new Event('resize'));
    }, 62.5);
  }

  /* ---------- Main Menu Open/Close, Min/Full ---------- */
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

  /* ---------- Disable moving to top ---------- */
  $('a[href="#"][data-top!=true]').click(function(e){
    e.preventDefault();
  });

});

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

/** 
 * Dynamic Modal
 * 
 * Based on: https://github.com/michaelsoriano/bootstrap-photo-gallery
 * 
 * Simplified & removed anchor click-event
 * **/
$.fn.bsPhotoGallery = function(options) {

    var settings = $.extend({}, $.fn.bsPhotoGallery.defaults, options);
    var id = generateId();
    var clicked = {};

    function getCurrentUl(){
      return 'ul[data-bsp-ul-id="'+clicked.ulId+'"][data-bsp-ul-index="'+clicked.ulIndex+'"]';
    }
    function generateId() {
      //http://fiznool.com/blog/2014/11/16/short-id-generation-in-javascript/
      var ALPHABET = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
      var ID_LENGTH = 4;
      var out = '';
      for (var i = 0; i < ID_LENGTH; i++) {
        out += ALPHABET.charAt(Math.floor(Math.random() * ALPHABET.length));
      }
      return 'bsp-'+out;
    }
    function createModalWrap(){

      if($('#bsPhotoGalleryModal').length !== 0){
        return false;
      }

      var modal = '';
      modal += '<div class="modal fade" id="bsPhotoGalleryModal" tabindex="-1" role="dialog"';
      modal += 'aria-labelledby="myModalLabel" aria-hidden="true">';
      modal += '<div class="modal-dialog modal-lg"><div class="modal-content">';
      modal += '<div class="modal-body"></div></div></div></div>';
      $('body').append(modal);

    }
    function showHideControls(){
  		var total = $(getCurrentUl()+' li[data-bsp-li-index]').length;

  		if(total === clicked.nextImg){
  			$('a.next').hide();
  		}else{
  			$('a.next').show()
  		}
  		if(clicked.prevImg === -1){
  			$('a.previous').hide();
  		}else{
  			$('a.previous').show()
  		}
  	}
    function showModal(){

        var src = $(this).find('img').attr('src');
        var largeImg = $(this).find('img').attr('data-bsp-large-src');
        if(typeof largeImg === 'string'){
              src = largeImg;
        }
        var index = $(this).attr('data-bsp-li-index');
        var ulIndex = $(this).parent('ul').attr('data-bsp-ul-index');
        var ulId = $(this).parent('ul').attr('data-bsp-ul-id');
        var theImg = $(this).find('img');
        var pText = $(this).find('.text').html();        
        var modalText = typeof pText !== 'undefined' ? pText : 'undefined';
        var alt =  typeof theImg.attr('alt') == 'string' ? theImg.attr('alt') : null;
        
        clicked.img = src;
        clicked.prevImg = parseInt(index) - parseInt(1);
    		clicked.nextImg = parseInt(index) + parseInt(1);
        clicked.ulIndex = ulIndex;
        clicked.ulId = ulId;


        $('#bsPhotoGalleryModal').modal();

        var html = '';
        var img = '<img src="' + clicked.img + '" class="img-responsive"/>';

        html += img;
        html += '<span class="' + settings.iconClose + ' bsp-close"></span>';
        html += '<div class="bsp-text-container">';
        
        if(alt !== null){
          html += '<h6>'+alt+'</h6>'
        }
        if(typeof pText !== 'undefined'){
          html += '<p class="pText">'+pText+'</p>'
        }        
        html += '</div>';
        html += '<a class="bsp-controls next" data-bsp-id="'+clicked.ulId+'" href="'+ (clicked.nextImg) + '"><span class="' + settings.iconRight + '"></span></a>';
        html += '<a class="bsp-controls previous" data-bsp-id="'+clicked.ulId+'" href="' + (clicked.prevImg) + '"><span class="' + settings.iconLeft + '"></span></a>';
      
        $('#bsPhotoGalleryModal .modal-body').html(html);
        $('.bsp-close').on('click', closeModal);
        showHideControls();
    }

    function closeModal(){
      $('#bsPhotoGalleryModal').modal('hide');
    }

    function nextPrevHandler(){

        var ul = $(getCurrentUl());
        var index = $(this).attr('href');

        var src = ul.find('li[data-bsp-li-index="'+index+'"] img').attr('src');
        var largeImg = ul.find('li[data-bsp-li-index="'+index+'"] img').attr('data-bsp-large-src');
        if(typeof largeImg === 'string'){
              src = largeImg;
        } 
        
        var pText = ul.find('li[data-bsp-li-index="'+index+'"] .text').html();        
        var modalText = typeof pText !== 'undefined' ? pText : 'undefined';
        var theImg = ul.find('li[data-bsp-li-index="'+index+'"] img');
        var alt =  typeof theImg.attr('alt') == 'string' ? theImg.attr('alt') : null;
         
        $('#bsPhotoGalleryModal .modal-body img').attr('src', src);
        var txt = '';
        if(alt !== null){
          txt += '<h6>'+alt+'</h6>'
        }
        if(typeof pText !== 'undefined'){
          txt += '<p class="pText">'+pText+'</p>'
        }        
        
        $('.bsp-text-container').html(txt); 

        clicked.prevImg = parseInt(index) - 1;
        clicked.nextImg = parseInt(clicked.prevImg) + 2;

        if($(this).hasClass('previous')){
            $(this).attr('href', clicked.prevImg);
            $('a.next').attr('href', clicked.nextImg);
        }else{
            $(this).attr('href', clicked.nextImg);
            $('a.previous').attr('href', clicked.prevImg);
        }
        // console.log(clicked);
      showHideControls();
      return false;
    }
    function clearModalContent(){
      $('#bsPhotoGalleryModal .modal-body').html('');
      clicked = {};
    }



    this.each(function(i){
      //ul
      var items = $(this).find('li');
      $(this).attr('data-bsp-ul-id', id);
      $(this).attr('data-bsp-ul-index', i);

      items.each(function(x){
    	$(this).find('a').on("click", function (e) {
            e.preventDefault();
        }); 
        var theImg = $(this).find('img'); 
        $(this).attr('data-bsp-li-index', x);
        theImg.addClass('img-responsive');
        if(settings.fullHeight){
          theImg.wrap('<div class="imgWrapper"></div>')
        }
        if(settings.hasModal === true){
          $(this).addClass('bspHasModal');
          $(this).on('click', showModal);
        }
      });
    })

    if(settings.hasModal === true){
      //this is for the next / previous buttons
      $(document).on('click', 'a.bsp-controls[data-bsp-id="'+id+'"]', nextPrevHandler);
      $(document).on('hidden.bs.modal', '#bsPhotoGalleryModal', clearModalContent);
      //start init methods
      createModalWrap();
    }

    return this;
};
/*defaults*/
$.fn.bsPhotoGallery.defaults = {
  'hasModal' : true, 
  'fullHeight' : true,
  'iconClose' : 'fa fa-times',
  'iconLeft' : 'fa fa-chevron-left',
  'iconRight' : 'fa fa-chevron-right'
}

$('ul.clearing-thumbs').bsPhotoGallery({});


// checkboxToFormHiddenInput

function checkboxToFormHiddenInput(checkboxElem, formSel) {
    jQuery(formSel + ' input[name="' + checkboxElem.name + '"]').remove();
    if (checkboxElem.checked) {
        jQuery(formSel).append('<input type="hidden" name="' + checkboxElem.name + '" value="' + checkboxElem.value + '" />')
    }
}
