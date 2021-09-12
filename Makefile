default: build

RM = rm -f
CP = cp

build: *.rkt
	raco exe --gui jumper.rkt

.PHONY: dist
dist: build
	raco distribute jumper/ jumper.exe
	$(CP) README.md jumper/
	$(CP) LICENSE jumper/

.PHONY: clean
clean:
	$(RM) jumper.exe
	$(RM) -r jumper
