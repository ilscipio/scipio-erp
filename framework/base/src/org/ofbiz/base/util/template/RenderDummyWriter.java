package org.ofbiz.base.util.template;

import java.io.IOException;
import java.io.Writer;

/**
 * SCIPIO: A Writer that discards all output, specific for rendering purposes.
 * <p>
 * We use a rendering-specific class in case we need to handle or log this output
 * differently.
 */
public class RenderDummyWriter extends Writer {

    private static final RenderDummyWriter INSTANCE = new RenderDummyWriter();

    /**
     * Default constructor.
     * NOTE: Usually preferable to use {@link #getSharedInstance()}.
     */
    public RenderDummyWriter() {
    }

    /**
     * Default constructor.
     * NOTE: Usually preferable to use {@link #getSharedInstance()}.
     */
    public RenderDummyWriter(Object lock) {
        super(lock);
    }
    
    /**
     * Returns a singleton-like shared instance. Usually this is preferable
     * to creating new instances, but it is valid to create separate instances where needed
     * in case the instances is needed like a sort of ID.
     */
    public static RenderDummyWriter getSharedInstance() {
        return INSTANCE;
    }

    @Override
    public void write(char[] cbuf, int off, int len) throws IOException {
    }

    @Override
    public void flush() throws IOException {
    }

    @Override
    public void close() throws IOException {
    }

    @Override
    public void write(int c) throws IOException {
    }

    @Override
    public void write(char[] cbuf) throws IOException {
    }

    @Override
    public void write(String str) throws IOException {
    }

    @Override
    public void write(String str, int off, int len) throws IOException {
    }

    @Override
    public Writer append(CharSequence csq) throws IOException {
        return this;
    }

    @Override
    public Writer append(CharSequence csq, int start, int end) throws IOException {
        return this;
    }

    @Override
    public Writer append(char c) throws IOException {
        return this;
    }

}
