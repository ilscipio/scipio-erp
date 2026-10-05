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
<@section>
    <#if productCategory?? && solrProducts?has_content>
        <#assign jsOptions>
            slidesToShow: ${viewCluster!4},
            slidesToScroll: ${viewScrollCluster!4},
            dots: true,
            respondTo : 'window',
            mobileFirst : false,
            rows:0,
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
                    <@render resource=productsummaryScreen reqAttribs={"productId": solrProduct.productId, "optProductId": solrProduct.productId,
                        "listIndex": solrProduct_index, "solrProduct": solrProduct} ctxVars={"solrProducts": solrProducts!{}, "solrProduct": solrProduct}/>
                </@slide>
            </#list>
        </@slider>
                    
    </#if>
    
</@section>
