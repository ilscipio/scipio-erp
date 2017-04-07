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
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<@section>
    <#if productCategory?? && solrProducts?has_content>
        <@paginate mode="content" layout="both" viewSize=(viewSize!1) viewIndex=(viewIndex!0) listSize=(listSize!0)>
            <@grid columns=4>
                <#list solrProducts as solrProduct>
                    <li><@render resource=productsummaryScreen reqAttribs={"productId":solrProduct.productId}/>
                    </li>
                </#list>
            </@grid>
        </@paginate>
    <#else>
        <@commonMsg type="result-norecord">${uiLabelMap.ProductNoProductsInThisCategory}</@commonMsg>
    </#if>
 </@section>   