/*
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
* */
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
