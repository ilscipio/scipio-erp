groovy:
                techDataCalendar = context.techDataCalendar;
                if (techDataCalendar == null && parameters.calendarId) {
                    techDataCalendar = delegator.findOne('TechDataCalendar', [calendarId:parameters.calendarId], false);
                }
                context.techDataCalendar = techDataCalendar;