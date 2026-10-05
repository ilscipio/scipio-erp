if (!context.applicationMenuLocation) { // automatically checks globalContext
                    applicationMenuLocation = application?.getAttribute("applicationMenuLocation");
                    if (applicationMenuLocation) {
                        globalContext.applicationMenuLocation = applicationMenuLocation;
                    }
                }
                if (!context.applicationMenuName) {
                    applicationMenuName = application?.getAttribute("applicationMenuName");
                    if (applicationMenuName) {
                        globalContext.applicationMenuName = applicationMenuName;
                    }
                }