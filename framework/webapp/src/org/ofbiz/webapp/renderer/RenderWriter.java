package org.ofbiz.webapp.renderer;

import java.io.IOException;
import java.io.Writer;

/**
 * SCIPIO: Special writers, mainly for targeted rendering.
 * <p>
 * WARN: these are NOT thread-safe, but they don't need to be in ofbiz rendering.
 */
public abstract class RenderWriter extends Writer {
    protected Writer origWriter;

    protected RenderWriter() {
        super();
        this.origWriter = null;
    }

    protected RenderWriter(Writer origWriter) {
        super();
        this.origWriter = origWriter;
    }

    public Writer getOrigWriter() {
        return origWriter;
    }

//    public void setOrigWriter(Writer origWriter) {
//        this.origWriter = origWriter;
//    }

    /**
     * Returns true if the writer is currently discarding output.
     */
    public abstract boolean isDiscarding();

    public abstract void beginSection(String name, String delimInfo) throws IOException;

    public abstract void endSection(String name, String delimInfo) throws IOException;


    /**
     * Returns true if the writer is RenderWriter and currently discarding output.
     */
    public static boolean isDiscarding(Appendable writer) {
        return (writer instanceof RenderWriter) && ((RenderWriter) writer).isDiscarding();
    }

    /**
     * Delegates to another writer, while keeping an optional reference to an original writer.
     * For reuse.
     */
    public static abstract class DelegRenderWriter extends RenderWriter {
        protected Writer targetWriter; // effective writer, must be initialized by sub-class

        protected DelegRenderWriter(Writer origWriter) {
            super(origWriter);
        }

        protected DelegRenderWriter(Writer origWriter, Writer targetWriter) {
            super(origWriter);
            this.targetWriter = targetWriter;
        }

//        public static DelegRenderWriter getInstance(Writer targetWriter, Writer origWriter) {
//            return new DelegRenderWriter(origWriter,
//                    targetWriter != null ? targetWriter : origWriter);
//        }
//
//        public static DelegRenderWriter getInstance(Writer targetWriter) {
//            return new DelegRenderWriter(null, targetWriter);
//        }

        @Override
        public boolean isDiscarding() {
            return (targetWriter instanceof DummyRenderWriter);
        }

        @Override
        public void write(int c) throws IOException {
            targetWriter.write(c);
        }

        @Override
        public void write(char[] cbuf) throws IOException {
            targetWriter.write(cbuf);
        }

        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
            targetWriter.write(cbuf, off, len);
        }

        @Override
        public void write(String str) throws IOException {
            targetWriter.write(str);
        }

        @Override
        public void write(String str, int off, int len) throws IOException {
            targetWriter.write(str, off, len);
        }

        @Override
        public Writer append(CharSequence csq) throws IOException {
            targetWriter.append(csq);
            return this;
        }

        @Override
        public Writer append(CharSequence csq, int start, int end) throws IOException {
            targetWriter.append(csq, start, end);
            return this;
        }

        @Override
        public Writer append(char c) throws IOException {
            targetWriter.append(c);
            return this;
        }

        @Override
        public void flush() throws IOException {
            targetWriter.flush();
        }

        @Override
        public String toString() {
            return targetWriter.toString();
        }

        @Override
        public void close() throws IOException {
            targetWriter.close();
        }
    }

    /**
     * Writer that writes and appends to one of two possible delegated writers - original or alternate - depending
     * on state flag. If not specified, the alt writer is set to a dummy writer that does nothing.
     * SPECIAL CASE: the {@link SwitchRenderWriter#flush()} and
     * {@link SwitchRenderWriter#close()} methods delegate BOTH writers.
     *
     * TODO: may want a fast multi-switch writer via begin/endSection calls to implement
     * {@link org.ofbiz.webapp.control.ViewAsJsonUtil#VIEWASJSONSPLITMODE_REQPARAM}.
     */
    public static class SwitchRenderWriter extends DelegRenderWriter {
        private Writer altWriter;
        private boolean master;

        protected SwitchRenderWriter(Writer origWriter, Writer altWriter, boolean stateUseOrig, boolean master) {
            super(origWriter);
            this.altWriter = altWriter;
            this.setState(stateUseOrig);
            this.master = master;
        }

        protected SwitchRenderWriter(Writer origWriter, boolean stateUseOrig, boolean master) {
            super(origWriter);
            this.altWriter = DummyRenderWriter.getDefaultInstance();
            this.setState(stateUseOrig);
            this.master = master;
        }

        /**
         * Creates with explicit alt (off) writer and initial state.
         */
        public static SwitchRenderWriter getInstance(Writer origWriter, Writer altWriter, boolean useOrigWriter, boolean master) {
            return new SwitchRenderWriter(origWriter, altWriter, useOrigWriter, master);
        }

        /**
         * Creates with dummy alt (off) writer and initial state.
         */
        public static SwitchRenderWriter getInstance(Writer origWriter, boolean useOrigWriter, boolean master) {
            return new SwitchRenderWriter(origWriter, useOrigWriter, master);
        }

        public void setState(boolean useOrigWriter) {
            this.targetWriter = useOrigWriter ? this.origWriter : this.altWriter;
        }

        public void useOrigWriter() {
            this.targetWriter = this.origWriter;
        }

        public void useAltWriter() {
            this.targetWriter = this.altWriter;
        }

        public Writer getAltWriter() {
            return altWriter;
        }

//        public void setAltWriter(Writer altWriter) {
//            this.altWriter = altWriter;
//        }

        public boolean isMaster() {
            return master;
        }

        @Override
        public void flush() throws IOException {
            // SPECIAL: applies to both writers.
            origWriter.flush();
            altWriter.flush();
        }

        @Override
        public void close() throws IOException {
            // SPECIAL: applies to both writers.
            origWriter.close();
            altWriter.close();
        }

        @Override
        public void beginSection(String name, String delimInfo) throws IOException {
//            if (master) {
//                // TODO: fast mode: switch the target writer based on name
//                // NOT IMPLEMENTED: fast mode not yet implemented because it is more likely to fail.
//            } else {
            RenderTargetUtil.makeMultiTargetDelimOpen(targetWriter, name, delimInfo);
//            }
        }

        @Override
        public void endSection(String name, String delimInfo) throws IOException {
//            if (master) {
//                // TODO: fast mode: switch the target writer based on name
//                // NOT IMPLEMENTED: fast mode not yet implemented because it is more likely to fail.
//            } else {
            RenderTargetUtil.makeMultiTargetDelimClose(targetWriter, name, delimInfo);
//            }
        }
    }

    /**
     * Dummy writer that outputs nothing.
     * Optionally can hold a reference to another writer for it to be passed around,
     * but they do not interact.
     */
    public static class DummyRenderWriter extends RenderWriter {
        private static final DummyRenderWriter DEFAULT_INSTANCE = new ImmutableDummyRenderWriter();

        protected DummyRenderWriter() {
            super();
        }

        protected DummyRenderWriter(Writer origWriter) {
            super(origWriter);
        }

        public static DummyRenderWriter getDefaultInstance() {
            return DEFAULT_INSTANCE;
        }

        public static DummyRenderWriter getInstance() {
            return new DummyRenderWriter();
        }

        public static DummyRenderWriter getInstance(Writer origWriter) {
            return new DummyRenderWriter(origWriter);
        }

        @Override
        public boolean isDiscarding() {
            return true;
        }

        @Override
        public void write(int c) throws IOException {
        }

        @Override
        public void write(char[] cbuf) throws IOException {
        }

        @Override
        public void write(char[] cbuf, int off, int len) throws IOException {
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

        @Override
        public void flush() throws IOException {
        }

        @Override
        public void close() throws IOException {
        }

        public static class ImmutableDummyRenderWriter extends DummyRenderWriter {
            protected ImmutableDummyRenderWriter() {
                super();
            }

//            @Override
//            public void setOrigWriter(Writer origWriter) {
//                throw new UnsupportedOperationException("cannot modify orig writer on ImmutableDummyRenderWriter");
//            }
        }

        @Override
        public void beginSection(String name, String delimInfo) throws IOException {
        }

        @Override
        public void endSection(String name, String delimInfo) throws IOException {
        }
    }

}
