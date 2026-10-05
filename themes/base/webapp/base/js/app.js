/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
// Foundation JavaScript
// Documentation can be found at: http://foundation.zurb.com/docs


function collapseFieldset(){
    var parent = $(".toggleField");
    parent.each(function( index ) {
        $(this).find("fieldset > .row").wrapAll('<div class="collapsehide"/>');
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
  if (typeof $(document).foundation() !== 'undefined'){
      $(document).foundation();
      scipioObjectFit();
      collapseFieldset();
      $('.ui-autocomplete').addClass('f-dropdown');

      if (typeof Pizza !== 'undefined') {
          Pizza.init(); // Create charts
      }
  }

    //jQuery validate specific options
    $.validator.setDefaults({
        errorElement: "span",
        errorClass: "error",
        debug: true
    });

});

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
