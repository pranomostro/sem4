TARGET = moep
TARGETJAR = $(TARGET).jar
CLASSFOLDER = classes
SOURCEFOLDER = src
CLASSES = Arguments Assignment1
JAVAFILES = $(addprefix src/, $(addsuffix .java, $(CLASSES)))

.PHONY: all clean

all: $(TARGET)

$(TARGETJAR): $(CLASSFOLDER) $(JAVAFILES)
	javac -encoding UTF-8 -classpath $(SOURCEFOLDER) -d $(CLASSFOLDER) $(JAVAFILES)
	@jar cfm $(TARGETJAR) deps/manifest.txt -C $(CLASSFOLDER) .

$(TARGET): $(TARGETJAR)
	@cp deps/run $(TARGET)
	@sed 's/\r$$//' -i $(TARGET)
	@chmod +x $(TARGET)

$(CLASSFOLDER):
	@mkdir -p $(CLASSFOLDER)

clean:
	-rm -rf $(CLASSFOLDER) $(TARGETJAR) $(TARGET)
