<#--
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
-->


<@section title="${uiLabelMap['OrderOrdersTotals']}">

       <#--
       <@cell columns="4"> 
            <@chart type="pie">
                <@chartdata value="36" title="Peperoni"/>
                <@chartdata value="2" title="Sausage"/> 
                <@chartdata value="19" title="Cheese"/> 
                <@chartdata value="6" title="Chicken"/> 
                <@chartdata value="27" title="Other"/>  
            </@chart>
        </@cell>
        -->
        
        <#--
        <@cell columns="4">
             <h3>Daily Order Item Counts</h3>
            <@chart type="line">
                <#list dailyStats.keySet() as day>
                    <@chartdata value="${dailyStats[day].count}" value2="${dailyStats[day].day}" title="${day!}"/>
                </#list>
            </@chart>
        </@cell>
        -->
        
        <@cell columns="4">
            <h3>Monthly Sales</h3>
            <@chart type="line">
                <#list monthlyStats.keySet() as key>
                    <@chartdata value="${monthlyStats[key].count}" title="${key}"/>
                </#list>  
            </@chart>
        </@cell>
        
        <@cell columns="4">
            <h3>Monthly Gross</h3>
            <@chart type="bar">
                <#list monthlyStats.keySet() as key>
                    <@chartdata value="${monthlyStats[key].total}" title="${key}"/>
                </#list>  
            </@chart>
        </@cell>
        
        <@cell columns="4">
             <h3>Weekly Sales</h3>
            <@chart type="line">
                <#list weeklyStats.keySet() as key>
                    <@chartdata value="${weeklyStats[key].count}" title="${key}"/>
                </#list> 
            </@chart>
        </@cell>

</@section>

<br/>
