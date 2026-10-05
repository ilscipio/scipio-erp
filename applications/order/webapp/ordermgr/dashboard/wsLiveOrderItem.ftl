<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<style>
.oiupdate{
  animation: oidupdated 5s linear forwards;
}

@keyframes oidupdated {
    0%   {color: #cae0c4;}
    100%   {color: inherit;}
}


</style>
<@script>
      var unlistedOrderIds = ['ORDER_REJECTED','ORDER_CANCELLED','ORDER_COMPLETED','ORDER_PACKED'];

      $(function(){
            var webSocketItem = new WebSocket('wss://' + window.location.host + '<@appUrl fullPath="false">/ws/orderitemdata/subscribe</@appUrl>');
            webSocketItem.onopen = function(event){
                var msg = {
                };
              webSocketItem.send(JSON.stringify(msg));
      };


            webSocketItem.onmessage = function(event){
                 var jsonObject, message;
                 var text = event.data;
                 message = text;
                  try {
                      jsonObject =  JSON.parse(text);
                      fillOrderItem(jsonObject);
                    } catch (error) {
                        console.log(error);
                    }
            };
      });

   function fillOrderItem(dataSet){
        var oid = $("#wsOrderItemData");
        oid.removeClass('oidupdate');
        oid.addClass('oiupdate');
        oid.find('.oidstatus').html(''+dataSet["status"]);
        oid.find('.oiditem').html(''+dataSet["orderItemSeqId"]);
        oid.find('.oidorder').html(''+dataSet["orderId"]);
        if(dataSet["gravatarImageURL"]){
            oid.find('.oidgravatar').html('<img src="'+dataSet["gravatarImageURL"]+'"/>');
        }else{
            oid.find('.oidgravatar').html('');
        }
   }
</@script>


<@section title=title!"" id="wsOrderItemData">
    <@pul class="oid">
        <@pli type="title" class="oidorder">N/A</@pli>
        <@pli class="oidgravatar">
        </@pli>
        <@pli type="description" class="oidstatus"></@pli>
        <@pli type="description" class="oiditem"></@pli>
    </@pul>
</@section>