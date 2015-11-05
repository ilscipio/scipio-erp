<#--
* 
* Informational/notification element HTML template include, default Cato markup.
*
* Included by htmlTemplate.ftl.
*
* NOTE: May have implicit dependencies on other parts of Cato API.
*
-->

<#-- 
*************
* Modal Macro
************
    Usage example:  
    <@modal id="dsadsa" attr="" >
    modal Content 
    </@modal>                
   * General Attributes *
    id              = set id (required)
    label           = set anchor text (required)
    icon            = generates icon inside the link (Note: has to be the full set of classes, e.g. "fa fa-fw fa-info")
-->
<#macro modal id label href="" icon="">
    <a href="#" data-reveal-id="${id}_modal" <#if href?has_content>data-reveal-ajax="${href!}"</#if>><#if icon?has_content><i class="${icon!}"></i> </#if>${label}</a>
    <div id="${id}_modal" class="${styles.modal_wrap!}" data-reveal>
        <#nested>
        <a class="close-reveal-modal">&#215;</a>
    </div>
</#macro>

<#-- 
*************
* Alert box
************
Alert box for messages that should grab user attention.
NOTE: Should avoid using this for regular, common inlined message results such as "no records found" unless
it's an unexpected result, error or one that requires user action. See other macros such as @resultMsg and @errorMsg.

    Usage example:  
    <@alert type="info">
        <#nested>
    </@alert>            
                    
   * General Attributes *
    type           = (info|success|warning|secondary|alert|error), default info
    class          = classes or additional classes for nested container
                     supports prefixes:
                       "+": causes the classes to append only, never replace defaults (same logic as empty string "")
                       "=": causes the class to replace non-essential defaults (same as specifying a class name directly)
-->
<#macro alert type="info" class="" id="">
<#local classes = compileClassArg(class, "${styles.grid_large!}12")>
<#local typeClass = "alert_type_${type!}"/>
<#if type="error"><#local type = "alert"></#if>
<div class="${styles.grid_row!}"<#if id?has_content> id="${id}"</#if>>
   <div class="${styles.grid_large!}12 ${styles.grid_cell!}">
       <div data-alert class="${styles.alert_wrap!} ${styles[typeClass]!}">
           <div class="${styles.grid_row!}">
              <div class="<#if classes?has_content>${classes} </#if>${styles.grid_cell!}">
                  <a href="#" class="close" data-dismiss="alert">&times;</a>
                  <#nested>
                  </div>
              </div>
           </div>
       </div>
   </div>
</#macro>

<#--
*************
* Panel box
************
    Usage example:  
    <@panel type="">
        <#nested>
    </@panel>            
                    
   * General Attributes *
    type           = (callout|) default:empty
    title          = Title
-->
<#macro panel type="" title="">
<div class="${styles.panel_wrap!} ${type}">
  <div class="${styles.panel_head!}"><#if title?has_content><h5 class="${styles.panel_title!}">${title!}</h5></#if></div>
  <div class="${styles.panel_body!}"><p><#nested></p></div>
</div>
</#macro>

<#-- 
*************
* Query result message
************
Common query result message wrapper.
Note: this is ONLY for expected, non-error messages, such as no records found in a query.
Other messages such as for missing params/record IDs are usually errors.

    Usage example:  
    <@resultMsg>${uiLabelMap.CommonNoRecordFound}.</@resultMsg>            
                    
   * General Attributes *
    class       = classes or additional classes for nested container
                  (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
-->
<#macro resultMsg class="" id="">
  <#local classes = compileClassArg(class, "result-msg")>
  <p<#if classes?has_content> class="${classes}"</#if><#if id?has_content> id="${id}"</#if>><#nested></p>
</#macro>

<#-- 
*************
* Error result message
************
Common error result message wrapper.
Abstracts/centralizes method used to display error, since of no consequence to most
templates: currently @alert.

    Usage example:  
    <@errorMsg type="permission">${uiLabelMap.CommonNoPermission}.</@errorMsg>            
                    
   * General Attributes *
    type           = [permission|security|error], default error
    class          = classes or additional classes for nested container
                     (if boolean, true means use defaults, false means prevent non-essential defaults; prepend with "+" to append-only, i.e. never replace non-essential defaults)
-->
<#macro errorMsg type="error" class="" id="">
  <@alert type="error" class=class id=id><#nested></@alert>
</#macro>

