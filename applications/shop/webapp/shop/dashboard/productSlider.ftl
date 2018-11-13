<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
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
