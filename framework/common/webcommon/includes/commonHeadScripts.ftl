
<#-- TODO: could remove output=true later and let accumulate in theme footer -->
<#if !styles.render_common_head_script?has_content || (styles.render_common_head_script)>
        <@script compress=true><#-- REMOVED: output=true because "merge" was added instead (duplicate function) -->
            <#-- Common Ofbiz URIs for use in javascript -->
            <@requireScriptOfbizUrl uri="getJSONuiLabelArray" onlyIfExists=true/>
            <@requireScriptOfbizUrl uri="getJSONuiLabel" onlyIfExists=true/>
            <#-- This belongs in @progressScript, but Ofbiz FTL bug requires it here -->
            <@requireScriptOfbizUrl uri="getFileUploadProgressStatus" onlyIfExists=true/>
            <@requireScriptOfbizUrl uri="getCountryList" onlyIfExists=true/>
            <@requireScriptOfbizUrl uri="getAssociatedStateList" onlyIfExists=true/>

            <@utilCache cacheName="commonHeadScripts.ftl" key="general::${globalContext.visualThemeId!}" expireTime=86400000>

                <#-- NOTE: a screen that needs a URL in JS must call @requireScriptOfbizUrl
                     FTL macro, for now, see htmlUtilities.ftl -->
                function getOfbizUrl(url, defUrl) {
                    if ((typeof commonOfbizUrls === 'object') && (url in commonOfbizUrls)) {
                        return commonOfbizUrls[url];
                    } else if (typeof defUrl !== 'undefined') {
                        return defUrl;
                    } else {
                        return "";
                    }
                }

                <#-- theme style variables
                     TODO?: could be optimized via static JS generated manually or cached -->
              <#if styles?has_content && styles.printStyleClassesOnPage!true>
                var scipioStyles = <@objectAsScript lang="js" object=styles />;
              </#if>
            </@utilCache>
              <#-- FIXME: DUPLICATED FROM messages.ftl; KEEP IN SYNC -->
              <#if requestAttributes.errorMessageList?has_content><#assign errorMessageList=requestAttributes.errorMessageList></#if>
              <#if requestAttributes.eventMessageList?has_content><#assign eventMessageList=requestAttributes.eventMessageList></#if>
              <#if requestAttributes.serviceValidationException??><#assign serviceValidationException = requestAttributes.serviceValidationException></#if>
              <#if requestAttributes.uiLabelMap?has_content><#assign uiLabelMap = requestAttributes.uiLabelMap></#if>

              <#if !errorMessage?has_content>
                <#assign errorMessage = requestAttributes._ERROR_MESSAGE_!>
              </#if>
              <#if !errorMessageList?has_content>
                <#assign errorMessageList = requestAttributes._ERROR_MESSAGE_LIST_!>
              </#if>
              <#if !eventMessage?has_content>
                <#assign eventMessage = requestAttributes._EVENT_MESSAGE_!>
              </#if>
              <#if !eventMessageList?has_content>
                <#assign eventMessageList = requestAttributes._EVENT_MESSAGE_LIST_!>
              </#if>


                function ScipioMessages(options) {
                    if (!options) {
                        options = {};
                    }

                    /* Public members */

                    this.isError = options.isError;
                    this.isErrorPage = options.isErrorPage;
                    this.errorMessage = options.errorMessage;
                    this.errorMessageList = options.errorMessageList;
                    this.eventMessage = options.eventMessage;
                    this.eventMessageList = options.eventMessageList;

                    /* Public functions */

                    this.hasErrorMsg = function() {
                        return (this.errorMessage || this.errorMessageList);
                    };
                }

                var scipioMsgs = new ScipioMessages({
                    isError : ${(isError!false)?string},   <#-- NOTE: this is official context var set by ScreenRenderer -->
                    isErrorPage : ${(isErrorPage!false)?string},   <#-- NOTE: this is some other check that was added in scipio at some point -->
                    errorMessage : <#if errorMessage?has_content>"${escapeVal(errorMessage, 'js')}"<#else>null</#if>,
                    errorMessageList : <#if errorMessageList?has_content><@objectAsScript object=errorMessageList lang='js'/><#else>[]</#if>,

                    eventMessage : <#if eventMessage?has_content>"${escapeVal(eventMessage, 'js')}"<#else>null</#if>,
                    eventMessageList : <#if eventMessageList?has_content><@objectAsScript object=eventMessageList lang='js'/><#else>[]</#if>
                });
        </@script>
</#if>