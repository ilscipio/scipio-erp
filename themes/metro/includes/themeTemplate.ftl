<#--
* 
* A set of HTML templating macros, part of standard Scipio Freemarker API.
* Automatically included at all times.
* Intended to be swappable.
*
* Overrides the default SCIPIO styles located in 
* htmlTemplate.ftl - ofbiz_foundation/framework/common/webcommon/scipio/lib/standard/
* 
-->

<#-- Master include: Includes the default macros and allows overrides -->
<#include "component://common/webcommon/includes/scipio/lib/standard/htmlTemplate.ftl"> 

<#-- 
*************
* MACRO OVERRIDES
************
See component://common/webcommon/includes/scipio/lib/standard/htmlTemplate.ftl for documentation about
the macro interfaces used in the standard macros and these overrides.
 -->

<#-- (currently no overrides) -->


<#-- save copy of this namespace so that our macros are able to access its own definitions without overrides (sometimes needed) -->
<#assign scipioMetroTmplLib = copyObject(.namespace)>
