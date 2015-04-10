.PHONY: all src man

src:
	$(MAKE) -C $@

man:
	- $(MAKE) -C $@ &> /dev/null

all: src man

