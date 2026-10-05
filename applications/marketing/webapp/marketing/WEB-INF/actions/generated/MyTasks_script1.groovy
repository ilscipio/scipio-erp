startDate = context.estimatedStartDate ?: context.actualStartDate ?: null;
                if (startDate) {
                    context.targetPeriodStart = org.ofbiz.base.util.UtilDateTime.toDateString(startDate, "yyyy-MM");
                } else {
                    context.targetPeriodStart = '';
                }