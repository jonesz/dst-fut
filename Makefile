test:
	futhark test lib/github.com/jonesz/dst/dst_test.fut

.PHONY: clean

clean:
	$(RM) \
	lib/github.com/jonesz/dst/dst_test \
	lib/github.com/jonesz/dst/bba_test \
	lib/github.com/jonesz/dst/*.c \
	lib/github.com/jonesz/dst/*.expected \
	lib/github.com/jonesz/dst/*.actual
