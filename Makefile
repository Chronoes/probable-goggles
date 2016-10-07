SQL?=./exec/db.sql

# pass to make as dbfile=<filename> for different name
dbfile?=db.sqlite

# pass to make as conffile=<filename> for different name
conffile?=server.conf

BIN_FOLDER=bin

.PHONY: server conf db

all: initdb server

bin:
	mkdir -p $(BIN_FOLDER)

server: bin conf
	cabal sandbox init
	cabal install -j
	cp ./.cabal-sandbox/bin/server ./$(BIN_FOLDER)

conf: bin
	@cat ./exec/server.conf.default | sed "s/db =.*/db = $(dbfile)/" > .server.conf
	@mv -i .server.conf ./$(BIN_FOLDER)/$(conffile)

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
