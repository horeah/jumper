default: build

RM = rm -f
CP = cp
TAR = tar

BUILD_DATE = $(shell date "+%y%m%d")

build: *.rkt
	raco exe --gui jumper.rkt

.PHONY: dist
dist: build
	raco distribute Jumper/ jumper.exe
	$(CP) README.md Jumper/
	$(CP) LICENSE Jumper/
	$(TAR) -a -cf Jumper-$(BUILD_DATE).zip Jumper

.PHONY: clean
clean:
	$(RM) jumper.exe
	$(RM) -r Jumper
