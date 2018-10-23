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
<@section>
    <#if productCategory?? && solrProducts?has_content>
        <#assign jsOptions>
            slidesToShow: ${viewCluster!4},
            slidesToScroll: ${viewScrollCluster!4},
            dots: true,
            respondTo : 'window',
            mobileFirst : false,
            focusOnSelect: true,
            lazyLoad: 'progressive',
            adaptiveHeight: false,
            useTransform:false,
            variableWidth:false,
             responsive: [
                {
                  breakpoint: 992,
                  settings: {
                    slidesToShow: 3,
                    slidesToScroll: 3,
                    infinite: true,
                    dots: true,
                    variableWidth:false
                  }
                },
                {
                  breakpoint: 768,
                  settings: {
                    slidesToShow: 2,
                    slidesToScroll: 2,
                    variableWidth:false
                  }
                },
                {
                  breakpoint: 544,
                  settings: 'unslick'
                }]
        </#assign>
        <@slider library="slick" jsOptions=jsOptions class="slider slides-${viewCluster!4}"> <#-- Relying on Slick Slider here - requires additional seed data.-->
            <#list solrProducts as solrProduct>
                <@slide library="slick">
                    <@render resource=productsummaryScreen reqAttribs={"productId": solrProduct.productId, "optProductId": solrProduct.productId, "listIndex": solrProduct_index} />
                </@slide>
            </#list>
        </@slider>
                    
    </#if>
    
</@section>
