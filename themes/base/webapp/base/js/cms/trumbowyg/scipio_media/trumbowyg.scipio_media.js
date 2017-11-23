/* ===========================================================
 * trumbowyg.scipio_media.js v1.0
 * Scipio Media Gallery Plugin for trumbowyg
 * ===========================================================
 */

(function ($) {
    'use strict';

    var defaultOptions = {
        serverPath: '/cms/control/getMediaFiles', // NOTE: caller must override
        mediaUrl: '/cms/media', // NOTE: caller must override
        data: [],
        statusPropertyName: 'success',
        success: undefined,
        error: undefined
    };
    
    // Custom function
    function getUrlVars(url) {
        var hash;
        var myJson = {};
        var hashes = url.slice(url.indexOf('?') + 1).split('&');
        for (var i = 0; i < hashes.length; i++) {
            hash = hashes[i].split('=');
            myJson[hash[0]] = hash[1];
        }
        return myJson;
    }
    
    // HTML Generation
    // Generate Modal content
    function getBodyContent(data,mediaUrl,errorMsg){
        var returnHtml = $('<div>',{style:'padding:10px;padding-top:0px;height:100%;'});
        var tableBody = $('<table>',{id: 'trumbowyg-table-container', class: 'table basic-table display',cellspacing:'0', width:'100%'})
        	.append($('<thead>',{})
        			.append($('<tr>')
        					.append($('<th>',{style:'width:24px;'}))
        					.append($('<th>',{}))
        			)        			
        	);
        returnHtml.append(tableBody);
        if(data.mediaFiles && data.mediaFiles.length >0){
                $.each(data.mediaFiles, function(i,item) {
                	
                	var itemBody = $('<tr>')
                	.append($('<td>',{}).append($('<input>', {type:'radio', id: item.contentId,value:'contentId='+item.contentId+'&dataResourceTypeId='+item.dataResourceTypeId, name:'dataResource'})))
                	.append($('<td>',{}).append($('<span>', {text: item.contentName})))                	
                	;
                	//itemContent.css('background-image','url('+mediaUrl+'?contentId='+item.contentId+'&dataResourceTypeId='+item.dataResourceTypeId+')');

                	tableBody.append(itemBody);
                });
        }else{
            returnHtml.append('<p>'+errorMsg+'</p>');
        }
        return returnHtml;
    }
    
    
    // Generate injected element
    function getMediaContent(data,mediaUrl){
        var returnHtml = '';
        
        $.each(data, function(i,item) {
            var value = item.value;
            var jsonValue = getUrlVars(value);
            switch(jsonValue.dataResourceTypeId) {
                case "AUDIO_OBJECT":
                    returnHtml+='<audio controls><source src="'+mediaUrl+'?'+value+'"></audio>';
                    break;
                case "DOCUMENT_OBJECT":
                    returnHtml+='<a href="'+mediaUrl+'?'+value+'" target="_blank">'+value+'</a>';
                    break;
                case "VIDEO_OBJECT":
                    returnHtml+='<video controls><source src="'+mediaUrl+'?'+value+'"></video>';
                    break;
                case "IMAGE_OBJECT":
                    returnHtml+='<img src="'+mediaUrl+'?'+value+'"/>';
                    break;
                default:
            }
        });
                
        return returnHtml;
    }

    // Plugin definition
    $.extend(true, $.trumbowyg, {
        langs: {
            en: {
                media: 'Media',
                error_no_media: 'No media asset found. Acces the media gallery to upload a new media asset.',
                insert_media_image: 'Insert Media: Image',
                insert_media_video: 'Insert Media: Video',
                insert_media_audio: 'Insert Media: Audio',
                insert_media_file: 'Insert Media: File'
            }
        },
        plugins: {
            scipio_media: {
                init: function (trumbowyg) {
                    trumbowyg.o.plugins.scipio_media = $.extend(true, {}, defaultOptions, trumbowyg.o.plugins.scipio_media || {});
                   
                    var openModalGallery= function (title, custom_html, cmd) {
                        var t = trumbowyg,
                        prefix = t.o.prefix,
                        lg = t.lang,
                        html = '',
                        CONFIRM_EVENT = 'tbwconfirm';
                        
                        html = custom_html;

                        return t.openModal(title, html)
                            .on(CONFIRM_EVENT, function () {
                                var $form = $('form', $(this)),
                                valid = true,
                                values = $form.serializeArray();
                                
                                t.restoreRange();
                                if (cmd(values)) {
                                    t.syncCode();
                                    t.$c.trigger('tbwchange');
                                    t.closeModal();
                                    $(this).off(CONFIRM_EVENT);
                                }

                            })
                            .one('tbwcancel', function () {
                                $(this).off(CONFIRM_EVENT);
                                t.closeModal();
                            });
                    }
                    
                    
                    // BUTTON Definitions
                    var scipioImageBtn = {
                        text: trumbowyg.lang.insert_media_image,
                        ico: 'insertImage',
                        fn: function () {
                            trumbowyg.saveRange();
                            var data = {'dataResourceTypeId':'IMAGE_OBJECT'};
                            $.ajax({
                                url: trumbowyg.o.plugins.scipio_media.serverPath,
                                type: 'POST',
                                data: data,
                                cache: false,
                                dataType: 'json',

                                success: function (data) {
                                    var bodyContent = getBodyContent(data,trumbowyg.o.plugins.scipio_media.mediaUrl,trumbowyg.lang.error_no_media);
                                    
                                    var $modal = openModalGallery(
                                        // Title
                                        trumbowyg.lang.media,

                                        // HTML
                                        bodyContent,

                                        // Callback
                                        function (v) {
                                            trumbowyg.execCmd('insertHTML', getMediaContent(v,trumbowyg.o.plugins.scipio_media.mediaUrl));
                                            setTimeout(function () {
                                                trumbowyg.closeModal();
                                            }, 250);
                                                    
                                            return true;
                                        }
                                    );
                                    
                                    //initialize datables
                                    $('#trumbowyg-table-container').DataTable( {
                                        'pagingType': 'simple',
                                        responsive: true,
                                        'lengthChange': false,
                                        'dom': '<f<t><p>><"clear">',
                                        'ordering': false,
                                        scrollY:        '200px',
                                        scrollCollapse: true,
                                        'pageLength': 5
                                    } );
                                },

                                error: trumbowyg.o.plugins.scipio_media.error || function () {
                                    trumbowyg.$c.trigger('tbwscipioerror', [trumbowyg]);
                                }
                            });
                            return true;
                        }
                    };
                    
                    var scipioVideoBtn = {
                            text: trumbowyg.lang.insert_media_video,
                            ico: 'insertImage',
                            fn: function () {
                                trumbowyg.saveRange();
                                var data = {'dataResourceTypeId':'VIDEO_OBJECT'};
                                $.ajax({
                                    url: trumbowyg.o.plugins.scipio_media.serverPath,
                                    type: 'POST',
                                    data: data,
                                    cache: false,
                                    dataType: 'json',

                                    success: function (data) {
                                        var bodyContent = getBodyContent(data,trumbowyg.o.plugins.scipio_media.mediaUrl,trumbowyg.lang.error_no_media);
                                        
                                        var $modal = openModalGallery(
                                            // Title
                                            trumbowyg.lang.media,

                                            // HTML
                                            bodyContent,

                                            // Callback
                                            function (v) {
                                                trumbowyg.execCmd('insertHTML', getMediaContent(v,trumbowyg.o.plugins.scipio_media.mediaUrl));
                                                setTimeout(function () {
                                                    trumbowyg.closeModal();
                                                }, 250);
                                                        
                                                return true;
                                            }
                                        );
                                        
                                        //initialize datables
                                        $('#trumbowyg-table-container').DataTable( {
                                            'pagingType': 'simple',
                                            responsive: true,
                                            'lengthChange': false,
                                            'dom': '<f<t><p>><"clear">',
                                            'ordering': false,
                                            "scrollY": "200px",
                                            scrollCollapse: true,
                                            'pageLength': 5
                                        } );
                                    },

                                    error: trumbowyg.o.plugins.scipio_media.error || function () {
                                        trumbowyg.$c.trigger('tbwscipioerror', [trumbowyg]);
                                    }
                                });
                                return true;
                            }
                        };
                    
                    var scipioAudioBtn = {
                            text: trumbowyg.lang.insert_media_audio,
                            ico: 'insertImage',
                            fn: function () {
                                trumbowyg.saveRange();
                                var data = {'dataResourceTypeId':'AUDIO_OBJECT'};
                                $.ajax({
                                    url: trumbowyg.o.plugins.scipio_media.serverPath,
                                    type: 'POST',
                                    data: data,
                                    cache: false,
                                    dataType: 'json',

                                    success: function (data) {
                                        var bodyContent = getBodyContent(data,trumbowyg.o.plugins.scipio_media.mediaUrl,trumbowyg.lang.error_no_media);
                                        
                                        var $modal = openModalGallery(
                                            // Title
                                            trumbowyg.lang.media,

                                            // HTML
                                            bodyContent,

                                            // Callback
                                            function (v) {
                                                trumbowyg.execCmd('insertHTML', getMediaContent(v,trumbowyg.o.plugins.scipio_media.mediaUrl));
                                                setTimeout(function () {
                                                    trumbowyg.closeModal();
                                                }, 250);
                                                        
                                                return true;
                                            }
                                        );
                                        
                                      //initialize datables
                                      $('#trumbowyg-table-container').DataTable( {
                                            'pagingType': 'simple',
                                            responsive: true,
                                            'lengthChange': false,
                                            'dom': '<f<t><p>><"clear">',
                                            'ordering': false,
                                            scrollY:        '200px',
                                            scrollCollapse: true,
                                            'pageLength': 5
                                        } );
                                    },

                                    error: trumbowyg.o.plugins.scipio_media.error || function () {
                                        trumbowyg.$c.trigger('tbwscipioerror', [trumbowyg]);
                                    }
                                });
                                return true;
                            }
                        };
                    
                    var scipioFileBtn = {
                            text: trumbowyg.lang.insert_media_file,
                            ico: 'insertImage',
                            fn: function () {
                                trumbowyg.saveRange();
                                var data = {'dataResourceTypeId':'DOCUMENT_OBJECT'};
                                $.ajax({
                                    url: trumbowyg.o.plugins.scipio_media.serverPath,
                                    type: 'POST',
                                    data: data,
                                    cache: false,
                                    dataType: 'json',

                                    success: function (data) {
                                        var bodyContent = getBodyContent(data,trumbowyg.o.plugins.scipio_media.mediaUrl,trumbowyg.lang.error_no_media);
                                        
                                        var $modal = openModalGallery(
                                            // Title
                                            trumbowyg.lang.media,

                                            // HTML
                                            bodyContent,

                                            // Callback
                                            function (v) {
                                                trumbowyg.execCmd('insertHTML', getMediaContent(v,trumbowyg.o.plugins.scipio_media.mediaUrl));
                                                setTimeout(function () {
                                                    trumbowyg.closeModal();
                                                }, 250);
                                                        
                                                return true;
                                            }
                                        );
                                        
                                      //initialize datables
                                      $('#trumbowyg-table-container').DataTable( {
                                            'pagingType': 'simple',
                                            responsive: true,
                                            'lengthChange': false,
                                            'dom': '<f<t><p>><"clear">',
                                            'ordering': false,
                                            scrollY:        '200px',
                                            scrollCollapse: true,
                                            'pageLength': 5
                                        } );
                                    },

                                    error: trumbowyg.o.plugins.scipio_media.error || function () {
                                        trumbowyg.$c.trigger('tbwscipioerror', [trumbowyg]);
                                    }
                                });
                                return true;
                            }
                        };

                    trumbowyg.addBtnDef('scipio_media_image', scipioImageBtn);
                    trumbowyg.addBtnDef('scipio_media_video', scipioVideoBtn);
                    trumbowyg.addBtnDef('scipio_media_audio', scipioAudioBtn);
                    trumbowyg.addBtnDef('scipio_media_file', scipioFileBtn);
                }
            }
        }
    });

    
})(jQuery);
