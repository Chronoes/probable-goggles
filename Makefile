SQL?=./conf/db.sql

# pass to make as dbfile=<filename> for different name
dbfile?=db.sqlite

# pass to make as conffile=<filename> for different name
conffile?=server.conf

BIN_FOLDER=bin

.PHONY: build-lib server conf db client

all: initdb conf server client

bin:
	mkdir -p $(BIN_FOLDER)

build-lib:
	cabal sandbox init
	cabal install -j

server: build-lib bin
	cp ./.cabal-sandbox/bin/server ./$(BIN_FOLDER)

client: build-lib bin
	cp ./.cabal-sandbox/bin/client ./$(BIN_FOLDER)

conf: bin
	@cat ./conf/server.conf.default | sed "s/db =.*/db = $(dbfile)/" > .server.conf
	@mv -i .server.conf ./$(BIN_FOLDER)/$(conffile)
	@rm -f .server.conf

initdb: bin
ifeq ($(shell [ -e $(BIN_FOLDER)/$(dbfile) ] && echo 1 || echo 0), 1)
	@echo "File $(BIN_FOLDER)/$(dbfile) already exists, remove it and run initdb again or run 'make forcedb'"
else
	$(MAKE) db
endif

forcedb: bin
	rm ./$(BIN_FOLDER)/$(dbfile)
	$(MAKE) db

db: bin
	sqlite3 ./$(BIN_FOLDER)/$(dbfile) < $(SQL)
	@echo "Created database: $(dbfile)"


clean:
	rm -rf .cabal-sandbox dist bin
