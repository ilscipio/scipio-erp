stepStyles = [:];
                for(stepState in context.setupStepStates?.values()) {
                    def styles = "";
                    // TODO?: translate via styles hash (may not matter much)
                    styles += (stepState?.completed) ? "menustep-complete" : "menustep-incomplete";
                    stepStyles[stepState.name] = styles;
                }
                context.stepStyles = stepStyles;