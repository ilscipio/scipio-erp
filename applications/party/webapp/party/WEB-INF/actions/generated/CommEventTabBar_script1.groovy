groovy:
                communicationEvent = context.communicationEvent;
                if (communicationEvent == null && parameters.communicationEventId) {
                    communicationEvent = delegator.findOne('CommunicationEvent', [communicationEventId: parameters.communicationEventId], false);
                } 
                context.CommEventTabBar_communicationEvent = communicationEvent;