all: test

test:
	futhark test \
	lib/github.com/jonesz/dst/dst_test.fut \
	lib/github.com/jonesz/dst/comb_test.fut

.PHONY: clean

clean:
	$(RM) \
	lib/github.com/jonesz/dst/*.c \
	lib/github.com/jonesz/dst/*.expected \
	lib/github.com/jonesz/dst/*.actual \
	lib/github.com/jonesz/dst/dst_test \
	lib/github.com/jonesz/dst/comb_test \
