TARGET=ethstats
TARGETJAR=$(TARGET).jar
CLASSFOLDER = classes
SOURCEFOLDER = src
CLASSES = Arguments GRNVS_RAW Timeout Assignment2
JAVAFILES = $(addprefix src/, $(addsuffix .java, $(CLASSES)))

# LIBRAW/GRNVS_SOCK required stuff
LIBSOCKET=deps/libGRNVS.so
LIBRAWDIR=deps/libraw/
LIBRAW=$(LIBRAWDIR)libraw.a
LDFLAGS=$(LIBRAW)
CFLAGS+=-I$(LIBRAWDIR)include -I/usr/lib/jvm/java-11-openjdk-amd64/include
CFLAGS+=-I/usr/lib/jvm/java-11-openjdk-amd64/include/linux -Ideps -fPIC
CFLAGS+=-fdiagnostics-color=auto

.PHONY: all clean fresh

all: $(TARGET) $(TARGETJAR) $(LIBSOCKET)

$(TARGETJAR): $(CLASSFOLDER)/.empty $(JAVAFILES)
	javac -encoding UTF-8 -classpath $(SOURCEFOLDER) -d $(CLASSFOLDER) $(JAVAFILES)
	@jar cfm $(TARGETJAR) deps/manifest.txt -C $(CLASSFOLDER) .


#sed replaces CRLF with just LF for windows + java users
$(TARGET): deps/run
	cp deps/run $(TARGET)
	@sed 's/\r$$//' -i $(TARGET)
	@chmod +x $(TARGET)

.PRECIOUS: $(CLASSFOLDER)/.empty
$(CLASSFOLDER)/.empty:
	@mkdir -p $(CLASSFOLDER)
	@touch $(CLASSFOLDER)/.empty

LIBSOCKETOBJECTS= deps/GRNVS_RAW.o

$(LIBSOCKET): $(LIBSOCKETOBJECTS) $(LIBRAW)
	$(CC) $(LIBSOCKETOBJECTS) $(LDFLAGS) -shared -o $(LIBSOCKET)

$(LIBRAW):
	$(MAKE) -C $(LIBRAWDIR)

clean:
	-$(MAKE) -C $(LIBRAWDIR) clean
	-rm -rf $(CLASSFOLDER)
	-rm -f $(TARGET)
	-rm -f $(LIBSOCKETOBJECTS)
	-rm -f $(LIBSOCKET)
	-rm -f $(TARGETJAR)

fresh: clean
	$(MAKE) all
