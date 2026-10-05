import com.ilscipio.scipio.cms.util.fileType.TikaUtil;
                    def mimeTypes;
                    // NOTE: CURRENTLY (2017-02-08) DOES NOT INCLUDE ALIASES (BY DEFAULT). TODO: REVIEW. 
                    boolean aliases = "true".equals(parameters.aliases)
                    if ("true".equals(parameters.missingOnly)) {
                        mimeTypes = TikaUtil.makeMissingEntityMimeTypes(delegator, aliases);
                    } else {
                        mimeTypes = TikaUtil.makeAllEntityMimeTypes(delegator, aliases);
                    }
                    if ("true".equals(parameters.sort)) {
                        mimeTypes = mimeTypes.toSorted { a, b -> a.mimeTypeId <=> b.mimeTypeId }
                    }
                    mimeTypes = mimeTypes.toUnique { a, b -> a.mimeTypeId <=> b.mimeTypeId }
                    context.mimeTypes = mimeTypes;