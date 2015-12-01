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

<div data-role="content">
<ul data-role="listview">
<li><a accesskey="1" href="<@ofbizUrl>/receipt?facilityId=${parameters.facilityId!}</@ofbizUrl>" class="${styles.link_nav!}">Goods Receipt</a></li>
<li><a accesskey="2" href="<@ofbizUrl>/movement?facilityId=${parameters.facilityId!}</@ofbizUrl>" class="${styles.link_nav!}">Inventory Movement</a></li>
<li><a accesskey="3" href="<@ofbizUrl>/picking?facilityId=${parameters.facilityId!}</@ofbizUrl>" class="${styles.link_nav!}">Picking</a></li>
<li><a accesskey="4" href="<@ofbizUrl>/packing?facilityId=${parameters.facilityId!}</@ofbizUrl>" class="${styles.link_nav!}">Packing</a></li>
<li><a accesskey="5" href="<@ofbizUrl>/stocktake?facilityId=${parameters.facilityId!}</@ofbizUrl>" class="${styles.link_nav!}">Stocktake</a></li>
</ul>
</div>

<#-- Setting Menu. -->
<div data-role="controlgroup">
  <a data-role="button" data-icon="gear" href="<@ofbizUrl>/menu</@ofbizUrl>" class="${styles.link_nav!}">Select Facility</a>
  <a data-role="button" data-icon="info" href="<@ofbizUrl>/logout</@ofbizUrl>" class="${styles.link_nav!}">Logout</a>
</div>
