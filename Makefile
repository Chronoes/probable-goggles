SQL?=exec/db.sql

# pass to make as dbfile=<filename> for different name
dbfile?=db.sqlite

# pass to make as conffile=<filename> for different name
conffile?=server.conf


.PHONY: server conf db

all: initdb server

server: conf
	cabal sandbox init
	cabal install -j
	cp dist/build/server/server ./

conf:
	@cp server.conf.default $(conffile)

initdb:
ifeq ($(shell [ -e $(dbfile) ] && echo 1 || echo 0), 1)
	@echo "File $(dbfile) already exists, remove it and run initdb again or run 'make forcedb'"
else
	$(MAKE) db
endif

forcedb:
	rm $(dbfile)
	$(MAKE) db

db:
	sqlite3 $(dbfile) < $(SQL)
	@echo "Created database: $(dbfile)"


clean:
	rm -r .cabal-sandbox dist
	rm -f server
