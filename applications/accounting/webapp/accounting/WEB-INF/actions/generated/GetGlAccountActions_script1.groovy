// SCIPIO: prevents problems with glAccountId!=null test, prevents empty string/undef
                        context.glAccountId = context.glAccountId ?: parameters.glAccountId ?: null;