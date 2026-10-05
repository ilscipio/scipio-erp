def title = context.title;
                def titleProperty = context.titleProperty;
                // Saved the orig "title" and "titleProperty" field JUST IN CASE something needs them; we may overwrite these
                context.origTitle = origTitle;
                context.origTitleProperty = titleProperty;

                // Choose whether using title or titleProperty
                def finalTitle = title;
                if (!finalTitle) {
                    def uiLabelMap = context.uiLabelMap;
                    if (titleProperty && uiLabelMap != null) {
                        finalTitle = uiLabelMap[titleProperty];
                    }
                }
                context.finalTitle = finalTitle;

                // Apply special title format expression (FSE)
                def titleFormat = context.titleFormat;
                def resolvedTitle;
                if (titleFormat && '${finalTitle}' != titleFormat) {
                    resolvedTitle = org.ofbiz.base.util.string.FlexibleStringExpander.expandString(titleFormat, context).trim();
                } else {
                    resolvedTitle = finalTitle;
                }
                context.resolvedTitle = resolvedTitle;

                // OVERWRITE the title field with resolved so automatically works with existing themes
                // NOTE: 2016-10-26: This now means that themes only need to check the "title" field; don't need to check titleProperty anymore (shouldn't be their job)
                context.title = resolvedTitle;
                // GLOBAL version of the title so top-level templates can also access it
                // (NOTE: due to language issues, must be a different field name)
                globalContext.globalTitle = resolvedTitle;
                context.headerTitle = resolvedTitle;

                if (!context.messagesTemplateLocation) {
                    context.messagesTemplateLocation = "component://common/webcommon/includes/messages.ftl"
                    globalContext.messagesTemplateLocation = context.messagesTemplateLocation
                }
                if (!context.loginTemplateLocation) {
                    context.loginTemplateLocation = "component://common/webcommon/login.ftl"
                    globalContext.loginTemplateLocation = context.loginTemplateLocation
                }
                if (!context.errorTemplateLocation) {
                    context.errorTemplateLocation = "component://common/webcommon/error.ftl"
                    globalContext.errorTemplateLocation = context.errorTemplateLocation
                }