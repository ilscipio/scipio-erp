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
<#assign maxLiveOrderEntries = maxLiveOrderEntries!UtilMisc.toIntegerObject(parameters.maxLiveOrderEntries!60)!60>
<@script>
      var maxLiveOrderEntries = ${maxLiveOrderEntries};
      $(function(){
            var webSocket = new WebSocket('wss://' + window.location.host + '<@appUrl fullPath="false">/ws/orderdatalive/subscribe</@appUrl>');
            webSocket.onopen = function(event){
                var msg = {
                };
              webSocket.send(JSON.stringify(msg));
              setTimeout(function() {
                    setInterval(timedUpdater, 60000); <#-- Run every minute -->
              }, ((60 - new Date().getMinutes()) * 60000)); <#-- start after an hour -->
            };


            webSocket.onmessage = function(event){
                 var jsonObject, message;
                 var text = event.data;
                 message = text;
                  try {
                      jsonObject =  JSON.parse(text);
                      var chart = $('#orderchart').data('chart');
                      var curIndex = chart.data.labels.indexOf(jsonObject.global.dateTimeStr)
                      if(curIndex > -1){
                         chart.data.datasets[0].data[curIndex] = jsonObject.global.totalAmount;
                         chart.data.datasets[1].data[curIndex] = jsonObject.global.totalOrders;
                      }else{
                         if (chart.data.labels.length >= maxLiveOrderEntries) {
                             <#-- Remove old data and add new -->
                             chart.data.labels.shift();
                             chart.data.datasets[0].data.shift();
                             chart.data.datasets[1].data.shift();
                         }
                         <#-- Add new element -->
                         chart.data.labels.push(jsonObject.global.dateTimeStr);
                         chart.data.datasets[0].data.push(jsonObject.global.totalAmount);
                         chart.data.datasets[1].data.push(jsonObject.global.totalOrders);
                      }
                      chart.update();
                    } catch (error) {
                        console.log(error);
                    }
            };

            function timedUpdater(){
                var time = moment("YYYY-MM-DD'T'HH:mm");
                var chart = $('#orderchart').data('chart');
                var curIndex = chart.data.labels.indexOf(time)
                if(curIndex == -1){
                     var chart = chart;
                     chart.data.labels.push(time);
                     chart.data.datasets[0].data.push(0);
                     chart.data.datasets[1].data.push(0);
                }
            }
      });
</@script>

<#assign currData=rewrapMap(orderStats!{}, "raw-simple")/>
<#assign fieldIdNum=fieldIdNum!0/>
<@section title=title!"">
    <@chart id="orderchart" label1=label1 label2=label2 xlabel=xlabel ylabel=ylabel type="bar">
        <#-- Additional chart options -->
        try {
            chart.config.options.scales.xAxes[0].ticks.display=false;
        } catch(e) {
        }
        <#-- Chart data -->
        <#if currData?has_content>
            <#list mapKeys(currData) as key>
                <#if chartType=="bar" || chartType=="line">
                    <@chartdata value=((currData[key].total)!0) value2=((currData[key].count)!0) title=(currData[key]["dateTime"])/>
                <#else>
                    <@chartdata value=((currData[key].total)!0) title=key/>
                </#if>
            </#list>
        </#if>
    </@chart>
</@section>