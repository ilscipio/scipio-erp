setupStep = context.setupStep;
                    partyId = parameters.partyId;
                    if (setupStep) {
                        context.useSetupWizardDec = true;
                        context.setupForce = true; // NOTE: This bypasses our own decorator checks, terrible
                        context.activeSubMenuItem = setupStep; // This is a hack to bypass the next PartyScreens include
                        context.setupSessionOrgSet = false; // Don't perturb session
                    } else {
                        // this assumes no support for anything but the wizard... true for now...
                        context.activeSubMenu = "TOP";
                        context.activeSubMenuItem = "wizard";
                    }
                    if (partyId && setupStep == "user") {
                        context.ecmSecTitleSuffix = ": " +
                            org.ofbiz.party.party.PartyHelper.getPartyName(delegator, partyId, false) +
                            " [" + partyId + "]";
                    }