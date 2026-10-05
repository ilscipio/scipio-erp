import org.ofbiz.webapp.website.WebSiteProperties
                import org.ofbiz.webapp.website.WebSiteEntityNotFoundException
                import org.ofbiz.base.util.Debug;
                import org.ofbiz.base.util.UtilProperties
                
                final String module = "CommonsScreens#GlobalActions";
                
                try {
                    Debug.logVerbose("############ BEGIN WEBSITE CHECK #############", module);                    
                    WebSiteProperties.from(request);
                    context.webSiteFound = true;
                    Debug.logVerbose("############ END WEBSITE CHECK   #############", module);
                } catch (WebSiteEntityNotFoundException we) {
                    Debug.logError("############ WEBSITE CHECK FAILED #############", module);
                    context.webSiteFound = false;
                    context.webSiteIdNotFound = we.getWebSiteId();
                    scipioBaseUrl = UtilProperties.getPropertyValue("general", "scipioerp.base.url", "");
                    scipioDevSetupUrl = UtilProperties.getPropertyValue("general", "scipioerp.ce.dev.setup", "");
                    context.scipioSetupUrl = scipioBaseUrl + scipioDevSetupUrl;                    
                    
                    request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage("CommonUiLabels", "CommonSystemNotProperlyConfigured", locale));
                }