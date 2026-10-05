vslResults = null;
                    if (parameters.vslPaths || parameters.vslClassNames) {
                        vslResults = dispatcher.runSync("validateSystemLocations", 
                            [userLogin:context.userLogin, paths: parameters.vslPaths, classNames: parameters.vslClassNames]);
                    }
                    context.vslResults = vslResults;