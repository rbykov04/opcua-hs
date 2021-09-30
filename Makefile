INCLUDES :=
INCLUDES += -Ilib/open62541/plugins/include
INCLUDES += -Ilib/open62541/include
INCLUDES += -Isrc
INCLUDES += -Ilib/open62541/build/src_generated
INCLUDES += -Ilib/open62541/arch
INCLUDES += -Ilib/open62541/deps

LIBS :=
LIBS += -Llib/open62541/build/bin -lopen62541

.PHONY: clean all run
all: uaclient

src/open62541/opcua.o: src/open62541/opcua.c
	gcc -o src/open62541/opcua.o  -c src/open62541/opcua.c $(INCLUDES)

src/open62541/Storable.hs: src/open62541/Storable.hsc
	hsc2hs src/open62541/Storable.hsc -o src/open62541/Storable.hs  $(INCLUDES)

HASKELL_FILES :=
HASKELL_FILES += src/open62541/Types.hs
HASKELL_FILES += src/open62541/Bindings.hs
HASKELL_FILES += src/open62541/Services.hs

.PHONY: opc-ua-client
opc-ua-client: src/open62541/Storable.hs
opc-ua-client: src/open62541/opcua.o
opc-ua-client: src/open62541/opcua.o

opc-ua-client: $(HASKELL_FILES)

run: examples/clientReadValue
	./examples/clientReadValue

clean:
	rm -rf examples/clientReadValue
	rm -rf *.o

test-c-opcua: src/open62541/test.c
test-c-opcua: src/open62541/opcua.o
	gcc -o test-c-opcua src/open62541/test.c src/open62541/opcua.o ${LIBS}

test-c-server: src/open62541/test-server.c
	gcc -o test-c-server src/open62541/test-server.c ${LIBS} $(INCLUDES)

test-c-server2: examples/test-server2.c
	gcc -o test-c-server2 examples/test-server2.c ${LIBS} $(INCLUDES)

examples/clientReadValue: examples/ClientReadValue.hs
examples/clientReadValue: opc-ua-client
	ghc  --make  -o examples/clientReadValue $(HASKELL_FILES)  examples/ClientReadValue.hs src/open62541/Storable.hs src/open62541/opcua.o ${LIBS}

uaclient: opc-ua-client
uaclient: src/cli/cli.hs
	ghc  --make  -o uaclient $(HASKELL_FILES)  src/cli/cli.hs src/open62541/Storable.hs src/open62541/opcua.o ${LIBS}
	./uaclient
