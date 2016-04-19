package com.ilscipio.cato.util;

import org.apache.tools.ant.Project;
import org.apache.tools.ant.listener.SilentLogger;

public class CatoLogger extends SilentLogger {
    
    @Override
    public void setMessageOutputLevel(int level) {
        super.setMessageOutputLevel(Project.MSG_ERR);
    }

}
